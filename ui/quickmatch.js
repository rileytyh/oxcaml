// ui/quickmatch.js
// Matchmaking + room sync (Firestore <-> OCaml bridge)
//
// IMPORTANT: This file does NOT hide #app.
// Your OCaml app renders both lobby + game inside #app,
// so we never toggle #app visibility here.

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

/**
 * Matchmaking model:
 * - queue/{doc}: { uid, status: waiting|matched|cancelled, matchedRoomId? }
 * - rooms/{room}: { players:{p1,p2}, by, stateSexp, updatedAt, createdAt }
 */
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
    await attachRoomSync(roomId);
    return { roomId, role: "white", rejoin: true };
  }

  if (p2 && p2 === uid) {
    env.roomId = roomId;
    env.role = "black";
    env.status = "matched";
    emitChanged(env);
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

  await attachRoomSync(roomId);
  return { roomId, role: "black" };
}

async function attachRoomSync(roomId) {
  const env = await waitSignedIn();
  const { db, uid, doc, onSnapshot, updateDoc, getDoc, serverTimestamp } = env;

  env.role = normalizeRole(env.role) || env.role;
  const roomRef = doc(db, "rooms", roomId);

  // Track last applied state by value (NOT by updatedAt ms).
  // This prevents "same-ms" serverTimestamp collisions from dropping updates.
  env._lastAppliedSexp = env._lastAppliedSexp || "";
  env._lastSentSexp = env._lastSentSexp || "";

  // ---- 0) Read current room ONCE to decide whether to seed initial state
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
    // conservative
    env.roomHasState = true;
    emitChanged(env);
  }

  // ---- 1) Send state to Firestore (called from OCaml via firebaseEnv.sendState)
  env.sendState = async (sexpString) => {
    if (!roomId) return;
    if (Date.now() < (env._suppressSendUntil || 0)) return;
    if (!truthyStr(sexpString)) return;

    // Drop exact duplicates (prevents spam when UI changes don't alter game state).
    if (sexpString === env._lastSentSexp) return;

    if (typeof navigator !== "undefined" && navigator.onLine === false) return;

    env._lastSentSexp = sexpString;

    await updateDoc(roomRef, {
      stateSexp: sexpString,
      by: uid,
      updatedAt: serverTimestamp(),
      // optional debug fields:
      // clientMs: Date.now(),
      // role: env.role || null,
    });

    if (!env.roomHasState) {
      env.roomHasState = true;
      emitChanged(env);
    }
  };

  window.firebaseEnv.sendState = env.sendState;

  // ---- 2) Seed initial state ONLY if:
  // - I am WHITE
  // - roomHasState is false (room not initialized yet)
  const tryRequestSend = () => {
    if (normalizeRole(env.role) !== "white") return true;
    if (env.roomHasState) return true; // IMPORTANT: do NOT reseed
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

  // ---- 3) unsubscribe old listener if any
  if (env._unsubRoom && typeof env._unsubRoom === "function") {
    try {
      env._unsubRoom();
    } catch {}
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

    // VALUE-based de-dupe (important!)
    if (sexp === env._lastAppliedSexp) return;

    const fromSelf = d.by && d.by === uid;

    // Apply if:
    // - from other player, OR
    // - we have not applied any room state yet (reload case)
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

// bootstrap: export API for OCaml
(async function bootstrap() {
  const env = await waitFirebaseBindings();
  env.quickmatch = quickmatch;
  env.createGame = createGame;
  env.joinGame = joinGame;
  env.attachRoomSync = attachRoomSync;

  // roomHasState is used to prevent white from overwriting existing games
  // and to allow exactly-once seeding for fresh rooms.
  if (typeof env.roomHasState === "undefined") env.roomHasState = true;

  emitChanged(env);
})();
