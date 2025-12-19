// ui/quickmatch.js

async function waitFirebaseReady(timeoutMs = 8000) {
  const start = Date.now();
  while (true) {
    const env = window.firebaseEnv;
    if (env && env.ready && env.uid && env.db) return env;
    if (Date.now() - start > timeoutMs) throw new Error("firebase not ready (timeout)");
    await new Promise((r) => setTimeout(r, 50));
  }
}

function el(tag, attrs = {}, text = "") {
  const n = document.createElement(tag);
  Object.entries(attrs).forEach(([k, v]) => n.setAttribute(k, v));
  if (text) n.textContent = text;
  return n;
}

function shortId(x, n = 6) {
  if (!x) return "";
  if (x.length <= n) return x;
  return x.slice(0, n) + "…" + x.slice(-2);
}

async function copyToClipboard(text) {
  try {
    await navigator.clipboard.writeText(text);
    return true;
  } catch {
    const t = document.createElement("textarea");
    t.value = text;
    document.body.appendChild(t);
    t.select();
    document.execCommand("copy");
    document.body.removeChild(t);
    return true;
  }
}

/**
 * Matchmaking:
 * - queue/{doc}: { uid, status: waiting|matched|cancelled, matchedRoomId? }
 * - rooms/{room}: { players:{p1,p2}, by, stateSexp, updatedAt }
 */
async function quickmatch() {
  const {
    db, uid, collection, doc, addDoc, updateDoc, getDocs,
    serverTimestamp, query, where, limit
  } = await waitFirebaseReady();

  const queueCol = collection(db, "queue");
  const roomsCol = collection(db, "rooms");

  // find waiting opponent
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
      stateSexp: null
    });

    await updateDoc(doc(db, "queue", opponent.queueDocId), {
      status: "matched",
      matchedRoomId: roomRef.id,
      matchedAt: serverTimestamp()
    });

    return { roomId: roomRef.id, role: "black" };
  }

  // no opponent: enqueue myself
  const myQueueRef = await addDoc(queueCol, {
    uid,
    status: "waiting",
    createdAt: serverTimestamp()
  });

  const start = Date.now();
  while (true) {
    const mineQ = query(queueCol, where("__name__", "==", myQueueRef.id), limit(1));
    const mineSnap = await getDocs(mineQ);
    const mine = mineSnap.docs[0]?.data();

    if (mine && mine.status === "matched" && mine.matchedRoomId) {
      return { roomId: mine.matchedRoomId, role: "white" };
    }

    if (Date.now() - start > 20000) {
      await updateDoc(doc(db, "queue", myQueueRef.id), { status: "cancelled" });
      throw new Error("Quickmatch timeout (20s)");
    }
    await new Promise((r) => setTimeout(r, 500));
  }
}

function prettyRole(role) {
  if (role === "white") return "White";
  if (role === "black") return "Black";
  return role;
}

async function setupUI() {
  const { db, uid, doc, onSnapshot, updateDoc, serverTimestamp } = await waitFirebaseReady();

  const panel = el("div", { id: "qm-panel" });
  Object.assign(panel.style, {
    position: "fixed",
    right: "16px",
    top: "16px",
    padding: "12px 14px",
    background: "rgba(0,0,0,0.78)",
    color: "white",
    borderRadius: "12px",
    fontFamily: "system-ui, -apple-system, sans-serif",
    zIndex: "99999",
    maxWidth: "360px",
    lineHeight: "1.35"
  });

  const title = el("div", {}, "Online Multiplayer");
  Object.assign(title.style, { fontWeight: "800", fontSize: "14px", marginBottom: "8px" });

  const signed = el("div", {}, `Signed in (Guest): ${shortId(uid, 6)}`);
  Object.assign(signed.style, { opacity: "0.9", fontSize: "12px", marginBottom: "8px" });

  const status = el("div", {}, "Status: idle");
  Object.assign(status.style, { fontSize: "12px", marginBottom: "8px" });

  const btn = el("button", {}, "Find Match");
  Object.assign(btn.style, {
    padding: "7px 10px",
    borderRadius: "10px",
    border: "1px solid #aaa",
    cursor: "pointer",
    fontWeight: "700",
    background: "#111",
    color: "#fff"
  });

  const roomLine = el("div", {}, "");
  Object.assign(roomLine.style, { marginTop: "10px", fontSize: "12px", opacity: "0.95" });

  const roleLine = el("div", {}, "");
  Object.assign(roleLine.style, { marginTop: "4px", fontSize: "12px", opacity: "0.95" });

  const oppLine = el("div", {}, "");
  Object.assign(oppLine.style, { marginTop: "6px", fontSize: "12px", opacity: "0.95" });

  const tips = el("div", {}, "");
  Object.assign(tips.style, { marginTop: "8px", fontSize: "11px", opacity: "0.75" });

  panel.append(title, signed, status, btn, roomLine, roleLine, oppLine, tips);
  document.body.appendChild(panel);

  let roomId = null;
  let role = null;
  let unsub = null;

  function renderRoomLine() {
    if (!roomId) { roomLine.textContent = ""; return; }
    roomLine.innerHTML = "";
    const label = el("span", {}, `Room: ${shortId(roomId, 6)} `);

    const copyBtn = el("button", {}, "Copy");
    Object.assign(copyBtn.style, {
      marginLeft: "6px",
      padding: "2px 6px",
      borderRadius: "8px",
      border: "1px solid #888",
      background: "#111",
      color: "#fff",
      cursor: "pointer",
      fontSize: "11px"
    });

    copyBtn.onclick = async () => {
      await copyToClipboard(roomId);
      tips.textContent = "Copied room id.";
      setTimeout(() => (tips.textContent = ""), 1200);
    };

    roomLine.append(label, copyBtn);
  }

  btn.onclick = async () => {
    try {
      btn.disabled = true;
      status.textContent = "Status: finding opponent…";

      const res = await quickmatch();
      roomId = res.roomId;
      role = res.role;

      window.firebaseEnv.roomId = roomId;
      window.firebaseEnv.role = role;

      status.textContent = "Status: matched ✅";
      renderRoomLine();
      roleLine.textContent = `You are: ${prettyRole(role)} (${role})`;

      // OCaml -> Firestore hook
      window.firebaseEnv.sendState = async (sexpString) => {
        if (!roomId) return;
        if (Date.now() < (window.firebaseEnv._suppressSendUntil || 0)) return;

        const roomRef = doc(db, "rooms", roomId);
        await updateDoc(roomRef, {
          stateSexp: sexpString,
          by: uid,
          updatedAt: serverTimestamp()
        });
      };

      // IMPORTANT: after match + sendState ready, ask OCaml to flush current state once
      if (window.ocamlRemote && typeof window.ocamlRemote.request_send === "function") {
        window.ocamlRemote.request_send();
      }

      // Firestore -> OCaml
      const roomRef = doc(db, "rooms", roomId);
      if (typeof unsub === "function") unsub();

      unsub = onSnapshot(roomRef, (snap) => {
        const d = snap.data();
        if (!d) return;

        if (d.by && d.by !== uid) oppLine.textContent = "Opponent moved.";
        else if (d.by === uid) oppLine.textContent = "You moved.";

        if (d.stateSexp && d.by && d.by !== uid) {
          // prevent immediate echo (best-effort)
          window.firebaseEnv._suppressSendUntil = Date.now() + 250;

          if (window.ocamlRemote && typeof window.ocamlRemote.set_state === "function") {
            window.ocamlRemote.set_state(d.stateSexp);
          } else {
            console.warn("ocamlRemote.set_state not found yet (did you add OCaml bridge?)");
          }
        }
      });
    } catch (e) {
      console.error(e);
      status.textContent = "Status: error (see console)";
      btn.disabled = false;
    }
  };
}

setupUI().catch(console.error);
