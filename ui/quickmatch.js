// ui/quickmatch.js
// Matchmaking + room sync + Invite flow + Analytics + light notifications
//
// IMPORTANT: This file does NOT hide #app.
// Your OCaml app renders both lobby + game inside #app.

// keep a set for notified invite ids
window.firebaseEnv = window.firebaseEnv || {};
window.firebaseEnv._notifiedInviteIds = window.firebaseEnv._notifiedInviteIds || new Set();

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

async function waitFirebaseBindings(timeoutMs = 8000) {
  const start = Date.now();
  while (true) {
    const env = window.firebaseEnv;
    const ok = env && env.db && env.auth && env.collection && env.doc;
    if (ok) return env;
    if (Date.now() - start > timeoutMs)
      throw new Error("firebaseEnv not ready (timeout)");
    await sleep(50);
  }
}

async function waitSignedIn(timeoutMs = 12000) {
  const env = await waitFirebaseBindings();
  const start = Date.now();
  while (true) {
    if (env.ready && env.uid) return env;
    if (Date.now() - start > timeoutMs) throw new Error("Not signed in (timeout)");
    await sleep(50);
  }
}

function emitChanged(env) {
  try {
    window.dispatchEvent(new CustomEvent("firebaseEnvChanged", { detail: env }));
  } catch {}
}

function truthyStr(x) {
  return typeof x === "string" && x.trim().length > 0;
}

function normalizeRole(x) {
  if (!x) return null;
  const s = String(x).trim().toLowerCase();
  if (s === "white" || s === "black") return s;
  return null;
}

function shortId(x, n = 6) {
  if (!x) return "—";
  const s = String(x);
  if (s.length <= n) return s;
  return s.slice(0, n) + "…" + s.slice(-2);
}

// ---------- Analytics wrapper ----------
function logEventName(env, name, params = undefined) {
  try {
    if (typeof env._analyticsLog === "function") env._analyticsLog(name, params || {});
  } catch {}
}

// ---------- Light notification wrapper ----------
async function requestNotificationPermission(env) {
  // Ensure notify() exists
  if (typeof env.notify !== "function") {
    env.notify = (title, opts = {}) => {
      try {
        if (typeof Notification === "undefined") return false;
        if (!env.notificationsEnabled) return false;
        if (Notification.permission !== "granted") return false;
        new Notification(title, opts || {});
        return true;
      } catch {
        return false;
      }
    };
  }

  if (typeof window === "undefined" || typeof Notification === "undefined") {
    env.notificationPermission = "unsupported";
    env.notificationsEnabled = false;
    emitChanged(env);
    return false;
  }

  try {
    // if already granted/denied, requestPermission may not pop; that's fine
    let p = Notification.permission;
    if (p !== "granted") {
      p = await Notification.requestPermission();
    }
    env.notificationPermission = p;
    env.notificationsEnabled = p === "granted";
    emitChanged(env);
    return env.notificationsEnabled;
  } catch (e) {
    env.notificationPermission = "error";
    env.notificationsEnabled = false;
    emitChanged(env);
    return false;
  }
}

function maybeNotifyInvite(env, inv) {
  try {
    if (typeof window === "undefined" || typeof Notification === "undefined") return;
    if (document && document.hidden !== true) return;
    if (Notification.permission !== "granted") return;
    if (!env.notificationsEnabled) return;

    const from = inv?.fromUid ? shortId(inv.fromUid) : "someone";
    new Notification("Backgammon invite", {
      body: `Invite from ${from}. Open tab to accept.`,
      tag: "bg-invite",
    });
  } catch {}
}

/**
 * Matchmaking model:
 * - queue/{doc}: { uid, status: waiting|matched|cancelled, matchedRoomId? }
 * - rooms/{room}: { players:{p1,p2}, by, stateSexp, updatedAt, createdAt }
 *
 * Invite model:
 * - invites/{inviteId}: {
 *     fromUid, toUid, roomId,
 *     status: "pending" | "accepted" | "declined",
 *     createdAt, respondedAt?
 *   }
 */

// -------------------- INVITES --------------------

async function createInvite(toUid) {
  const env = await waitSignedIn();
  const { db, uid, collection, addDoc, serverTimestamp } = env;

  if (!truthyStr(toUid)) throw new Error("toUid required");
  if (toUid === uid) throw new Error("Cannot invite yourself");

  // Create a dedicated room for this invite (host becomes White)
  const room = await createGame();
  const roomId = room.roomId;

  const invitesCol = collection(db, "invites");
  const inviteRef = await addDoc(invitesCol, {
    fromUid: uid,
    toUid,
    roomId,
    status: "pending",
    createdAt: serverTimestamp(),
  });

  env.lastInviteId = inviteRef.id;
  env.lastInvitedTo = toUid;

  // Start listening so inviter auto-enters game when accepted
  try {
    await listenInviteStatus(inviteRef.id);
  } catch {}

  logEventName(env, "invite_sent", { to_uid: toUid });
  emitChanged(env);

  return { inviteId: inviteRef.id, roomId };
}

async function acceptInvite(inviteId) {
  const env = await waitSignedIn();
  const { db, uid, doc, getDoc, updateDoc, serverTimestamp } = env;

  if (!truthyStr(inviteId)) throw new Error("inviteId required");

  const invRef = doc(db, "invites", inviteId);
  const snap = await getDoc(invRef);
  if (!snap.exists()) throw new Error("Invite not found");

  const inv = snap.data() || {};
  if (inv.toUid !== uid) throw new Error("Not your invite");
  if (!truthyStr(inv.roomId)) throw new Error("Invite missing roomId");

  await updateDoc(invRef, {
    status: "accepted",
    respondedAt: serverTimestamp(),
  });

  // Join room as Black
  await joinGame(inv.roomId);

  env.lastAcceptedInviteId = inviteId;

  logEventName(env, "invite_accepted", { room_id: inv.roomId });
  logEventName(env, "game_start", { via: "invite_accept" });

  emitChanged(env);
  return { ok: true, roomId: inv.roomId };
}

async function declineInvite(inviteId) {
  const env = await waitSignedIn();
  const { db, uid, doc, getDoc, updateDoc, serverTimestamp } = env;

  if (!truthyStr(inviteId)) throw new Error("inviteId required");

  const invRef = doc(db, "invites", inviteId);
  const snap = await getDoc(invRef);
  if (!snap.exists()) throw new Error("Invite not found");

  const inv = snap.data() || {};
  if (inv.toUid !== uid) throw new Error("Not your invite");

  await updateDoc(invRef, {
    status: "declined",
    respondedAt: serverTimestamp(),
  });

  env.lastDeclinedInviteId = inviteId;
  emitChanged(env);
  return { ok: true };
}

/**
 * Listen incoming invites for current user (pending only).
 * Updates env.incomingInvites = [{id, ...data}]
 * Also triggers Notification when new invite arrives and tab is hidden.
 */
async function listenIncomingInvites() {
  const env = await waitSignedIn();
  const { db, uid, collection, query, where, onSnapshot } = env;

  // cleanup old
  if (env._unsubInvites && typeof env._unsubInvites === "function") {
    try { env._unsubInvites(); } catch {}
  }

  const invitesCol = collection(db, "invites");
  const q = query(
    invitesCol,
    where("toUid", "==", uid),
    where("status", "==", "pending")
  );

  env.incomingInvites = env.incomingInvites || [];
  env._knownInviteIds = env._knownInviteIds || {};
  env._notifiedInviteIds = env._notifiedInviteIds || new Set();
  emitChanged(env);

  env._unsubInvites = onSnapshot(q, (snap) => {
    const arr = [];
    for (const d of snap.docs) {
      const data = d.data() || {};
      arr.push({ id: d.id, ...data });
    }
    arr.sort((a, b) => {
      const ta = a.createdAt?.seconds || 0;
      const tb = b.createdAt?.seconds || 0;
      return tb - ta;
    });

    // detect new invites
    const prev = env._knownInviteIds || {};
    const next = {};
    let firstNew = null;
    for (const inv of arr) {
      next[inv.id] = true;
      if (!prev[inv.id] && !firstNew) firstNew = inv;
    }
    env._knownInviteIds = next;

    env.incomingInvites = arr;
    emitChanged(env);

    // Check for new invites and send notifications
    for (const inv of env.incomingInvites || []) {
      if (!inv || !inv.id) continue;
      if (env._notifiedInviteIds.has(inv.id)) continue;

      // mark as seen
      env._notifiedInviteIds.add(inv.id);

      // only notify when tab is hidden + permission granted
      if (document.hidden && env.notificationsEnabled && Notification.permission === "granted") {
        env.notify("Backgammon invite", {
          body: `From ${inv.fromUid ? inv.fromUid.slice(0,6) + "…" : "someone"}`,
        });
        env.logEventWith?.("invite_notification", { invite_id: inv.id });
      }
    }

    if (firstNew) {
      maybeNotifyInvite(env, firstNew);
    }
  });

  return true;
}

/**
 * Listen outgoing invite status so inviter can detect accept/decline
 * and automatically enter game on accept.
 */
async function listenInviteStatus(inviteId) {
  const env = await waitSignedIn();
  const { db, uid, doc, onSnapshot } = env;
  if (!truthyStr(inviteId)) throw new Error("inviteId required");

  if (env._unsubInviteStatus && typeof env._unsubInviteStatus === "function") {
    try { env._unsubInviteStatus(); } catch {}
  }

  const invRef = doc(db, "invites", inviteId);
  env._unsubInviteStatus = onSnapshot(invRef, (snap) => {
    if (!snap.exists()) return;
    const inv = snap.data() || {};
    if (inv.fromUid !== uid) return;

    const st = inv.status || null;
    env.lastInviteStatus = st;
    emitChanged(env);

    if (st === "accepted") {
      // inviter is host (white) already in roomId from createGame()
      // Switch UI into game page
      env.status = "matched";
      env.role = normalizeRole(env.role) || env.role;
      emitChanged(env);

      logEventName(env, "matched", { via: "invite_accepted" });
      logEventName(env, "game_start", { via: "invite_host" });
    }
  });

  return true;
}

// -------------------- QUICKMATCH / ROOMS --------------------

async function quickmatch() {
  const env = await waitSignedIn();
  const {
    db,
    uid,
    collection,
    doc,
    addDoc,
    updateDoc,
    getDocs,
    getDoc,
    serverTimestamp,
    query,
    where,
    limit,
  } = env;

  logEventName(env, "quickmatch_start");

  env.status = "waiting";
  emitChanged(env);

  const queueCol = collection(db, "queue");
  const roomsCol = collection(db, "rooms");

  // 1) find waiting opponent
  const q = query(queueCol, where("status", "==", "waiting"), limit(10));
  const snap = await getDocs(q);

  let opponent = null;
  for (const d of snap.docs) {
    const data = d.data();
    if (data.uid && data.uid !== uid) {
      opponent = { queueDocId: d.id, uid: data.uid };
      break;
    }
  }

  if (opponent) {
    // I become p2 (Black)
    const roomRef = await addDoc(roomsCol, {
      createdAt: serverTimestamp(),
      updatedAt: serverTimestamp(),
      players: { p1: opponent.uid, p2: uid },
      by: uid,
      stateSexp: null,
    });

    await updateDoc(doc(db, "queue", opponent.queueDocId), {
      status: "matched",
      matchedRoomId: roomRef.id,
      matchedAt: serverTimestamp(),
    });

    env.roomId = roomRef.id;
    env.role = "black";
    env.status = "matched";
    emitChanged(env);

    logEventName(env, "matched", { via: "quickmatch" });
    logEventName(env, "game_start", { via: "quickmatch" });

    await attachRoomSync(roomRef.id);
    return { roomId: roomRef.id, role: "black" };
  }

  // 2) no opponent: enqueue myself
  const myQueueRef = await addDoc(queueCol, {
    uid,
    status: "waiting",
    createdAt: serverTimestamp(),
  });

  const start = Date.now();
  while (true) {
    const mineSnap = await getDoc(doc(db, "queue", myQueueRef.id));
    const mine = mineSnap.exists() ? mineSnap.data() : null;

    if (mine && mine.status === "matched" && mine.matchedRoomId) {
      env.roomId = mine.matchedRoomId;
      env.role = "white";
      env.status = "matched";
      emitChanged(env);

      logEventName(env, "matched", { via: "quickmatch" });
      logEventName(env, "game_start", { via: "quickmatch" });

      await attachRoomSync(mine.matchedRoomId);
      return { roomId: mine.matchedRoomId, role: "white" };
    }

    if (Date.now() - start > 20000) {
      await updateDoc(doc(db, "queue", myQueueRef.id), { status: "cancelled" });
      env.status = "error";
      emitChanged(env);
      throw new Error("Quickmatch timeout (20s)");
    }

    await sleep(500);
  }
}

// Create a room you can share (host becomes White)
async function createGame() {
  const env = await waitSignedIn();
  const { db, uid, collection, addDoc, serverTimestamp } = env;

  const roomsCol = collection(db, "rooms");
  const roomRef = await addDoc(roomsCol, {
    createdAt: serverTimestamp(),
    updatedAt: serverTimestamp(),
    players: { p1: uid, p2: null },
    by: uid,
    stateSexp: null,
  });

  env.roomId = roomRef.id;
  env.role = "white";
  env.status = "waiting";
  emitChanged(env);

  await attachRoomSync(roomRef.id);
  return { roomId: roomRef.id, role: "white", created: true };
}

// Join a room (joiner becomes Black if slot open)
async function joinGame(roomId) {
  const env = await waitSignedIn();
  const { db, uid, doc, getDoc, updateDoc, serverTimestamp } = env;

  const roomRef = doc(db, "rooms", roomId);
  const snap = await getDoc(roomRef);
  if (!snap.exists()) throw new Error("Room not found");

  const data = snap.data() || {};
  const players = data.players || {};
  const p1 = players.p1 || null;
  const p2 = players.p2 || null;

  // Rejoin cases
  if (p1 && p1 === uid) {
    env.roomId = roomId;
    env.role = "white";
    env.status = "matched";
    emitChanged(env);
    logEventName(env, "game_start", { via: "rejoin" });
    await attachRoomSync(roomId);
    return { roomId, role: "white", rejoin: true };
  }

  if (p2 && p2 === uid) {
    env.roomId = roomId;
    env.role = "black";
    env.status = "matched";
    emitChanged(env);
    logEventName(env, "game_start", { via: "rejoin" });
    await attachRoomSync(roomId);
    return { roomId, role: "black", rejoin: true };
  }

  // Fill empty slot
  if (!p1) {
    await updateDoc(roomRef, { "players.p1": uid, updatedAt: serverTimestamp() });
    env.roomId = roomId;
    env.role = "white";
    env.status = "matched";
    emitChanged(env);
    logEventName(env, "game_start", { via: "join_room" });
    await attachRoomSync(roomId);
    return { roomId, role: "white" };
  }

  if (p2) throw new Error("Room already has two players");

  await updateDoc(roomRef, {
    "players.p2": uid,
    updatedAt: serverTimestamp(),
  });

  env.roomId = roomId;
  env.role = "black";
  env.status = "matched";
  emitChanged(env);

  logEventName(env, "game_start", { via: "join_room" });

  await attachRoomSync(roomId);
  return { roomId, role: "black" };
}

async function attachRoomSync(roomId) {
  const env = await waitSignedIn();
  const { db, uid, doc, onSnapshot, updateDoc, getDoc, serverTimestamp } = env;

  env.role = normalizeRole(env.role) || env.role;
  const roomRef = doc(db, "rooms", roomId);

  env._lastAppliedSexp = env._lastAppliedSexp || "";
  env._lastSentSexp = env._lastSentSexp || "";

  // ---- 0) Read current room ONCE
  try {
    const s0 = await getDoc(roomRef);
    const d0 = s0.exists() ? (s0.data() || {}) : {};
    const hasState0 = truthyStr(d0.stateSexp);

    env.roomHasState = !!hasState0;
    env._seenRemote = env._seenRemote || false;

    if (hasState0 && window.ocamlRemote && typeof window.ocamlRemote.set_state === "function") {
      env._suppressSendUntil = Date.now() + 1200;
      env._seenRemote = true;
      env._lastAppliedSexp = d0.stateSexp;
      emitChanged(env);

      window.ocamlRemote.set_state(d0.stateSexp);
    } else {
      emitChanged(env);
    }
  } catch {
    env.roomHasState = true;
    emitChanged(env);
  }

  // ---- 1) Send state
  env.sendState = async (sexpString) => {
    if (!roomId) return;
    if (Date.now() < (env._suppressSendUntil || 0)) return;
    if (!truthyStr(sexpString)) return;
    if (sexpString === env._lastSentSexp) return;
    if (typeof navigator !== "undefined" && navigator.onLine === false) return;

    env._lastSentSexp = sexpString;

    await updateDoc(roomRef, {
      stateSexp: sexpString,
      by: uid,
      updatedAt: serverTimestamp(),
    });

    if (!env.roomHasState) {
      env.roomHasState = true;
      emitChanged(env);
    }
  };

  window.firebaseEnv.sendState = env.sendState;

  // ---- 2) Seed initial state only if white and room not initialized
  const tryRequestSend = () => {
    if (normalizeRole(env.role) !== "white") return true;
    if (env.roomHasState) return true;
    if (window.ocamlRemote && typeof window.ocamlRemote.request_send === "function") {
      window.ocamlRemote.request_send();
      return true;
    }
    return false;
  };

  if (!tryRequestSend()) {
    const start = Date.now();
    const timer = setInterval(() => {
      if (tryRequestSend() || Date.now() - start > 5000) clearInterval(timer);
    }, 80);
  }

  // ---- 3) unsubscribe old listener
  if (env._unsubRoom && typeof env._unsubRoom === "function") {
    try { env._unsubRoom(); } catch {}
  }

  env._seenRemote = env._seenRemote || false;

  env._unsubRoom = onSnapshot(roomRef, (snap) => {
    const d = snap.data();
    if (!d) return;
    if (snap.metadata && snap.metadata.hasPendingWrites) return;

    const sexp = d.stateSexp;
    const hasState = truthyStr(sexp);

    if (hasState && !env.roomHasState) {
      env.roomHasState = true;
      emitChanged(env);
    }

    if (!hasState) return;
    if (sexp === env._lastAppliedSexp) return;

    const fromSelf = d.by && d.by === uid;

    if (!fromSelf || !env._seenRemote) {
      env._lastAppliedSexp = sexp;
      env._suppressSendUntil = Date.now() + 1200;
      env._seenRemote = true;
      emitChanged(env);

      if (window.ocamlRemote && typeof window.ocamlRemote.set_state === "function") {
        window.ocamlRemote.set_state(sexp);
      } else {
        console.warn("[quickmatch] ocamlRemote.set_state not found yet");
      }
    }
  });

  return true;
}

// bootstrap: export API for OCaml + start invite listener
(async function bootstrap() {
  const env = await waitFirebaseBindings();

  // existing API
  env.quickmatch = quickmatch;
  env.createGame = createGame;
  env.joinGame = joinGame;
  env.attachRoomSync = attachRoomSync;

  // Invite API
  env.createInvite = createInvite;
  env.acceptInvite = acceptInvite;
  env.declineInvite = declineInvite;
  env.listenIncomingInvites = listenIncomingInvites;
  env.listenInviteStatus = listenInviteStatus;

  // analytics API for OCaml UI
  env.logEvent = (name) => logEventName(env, name);

  // notifications API for OCaml UI
  if (typeof Notification !== "undefined") {
    env.notificationPermission = Notification.permission;

    // If user already allowed notifications previously, auto-enable.
    if (typeof env.notificationsEnabled !== "boolean") {
      env.notificationsEnabled = Notification.permission === "granted";
    }

    // Provide env.notify() for both OCaml UI + quickmatch.js
    if (typeof env.notify !== "function") {
      env.notify = (title, opts = {}) => {
        try {
          if (!env.notificationsEnabled) return false;
          if (Notification.permission !== "granted") return false;
          new Notification(title, opts || {});
          return true;
        } catch {
          return false;
        }
      };
    }
  } else {
    env.notificationPermission = "unsupported";
    env.notificationsEnabled = false;
  }

  env.requestNotificationPermission = () => requestNotificationPermission(env);

  if (typeof env.roomHasState === "undefined") env.roomHasState = true;

  // Kick off invite inbox listener automatically once signed in
  try {
    await waitSignedIn();
    logEventName(env, "sign_in"); // if sign-in already happened by the time quickmatch loads
    await listenIncomingInvites();
  } catch (e) {
    console.warn("[quickmatch] invite listener not started:", e?.message || e);
  }

  emitChanged(env);
})();
