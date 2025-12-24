// ui/friends.js
// Minimal invites API:
// - setProfile({displayName, handle})
// - sendInvite(toHandleOrUid, roomId)
// - watchInvites(onInvite)

(function () {
  async function waitEnv(timeoutMs = 8000) {
    const start = Date.now();
    while (true) {
      const env = window.firebaseEnv;
      if (env && env.db && env.uid && env.collection && env.doc) return env;
      if (Date.now() - start > timeoutMs) throw new Error("firebaseEnv not ready");
      await new Promise((r) => setTimeout(r, 50));
    }
  }

  async function setProfile({ displayName, handle }) {
    const env = await waitEnv();
    const { db, uid, doc, setDoc, serverTimestamp } = env;
    await setDoc(doc(db, "users", uid), {
      displayName: displayName || null,
      handle: handle || null,
      updatedAt: serverTimestamp(),
      createdAt: serverTimestamp()
    }, { merge: true });

    env.analyticsLog && env.analyticsLog("profile_set", { has_handle: !!handle });
  }

  async function resolveUserByHandleOrUid(q) {
    const env = await waitEnv();
    const { db, collection, getDocs, query, where, limit, uid } = env;
    if (!q) return null;

    // if looks like uid, try direct
    if (q.length >= 20) return q === uid ? null : q;

    // else treat as handle
    const usersCol = collection(db, "users");
    const snap = await getDocs(query(usersCol, where("handle", "==", q), limit(1)));
    if (snap.empty) return null;
    const d = snap.docs[0];
    return d.id;
  }

  async function sendInvite(toHandleOrUid, roomId) {
    const env = await waitEnv();
    const { db, uid, collection, addDoc, serverTimestamp } = env;

    const toUid = await resolveUserByHandleOrUid(toHandleOrUid);
    if (!toUid) throw new Error("User not found");

    const invitesCol = collection(db, "invites");
    const ref = await addDoc(invitesCol, {
      fromUid: uid,
      toUid,
      roomId,
      status: "pending",
      createdAt: serverTimestamp()
    });

    env.analyticsLog && env.analyticsLog("invite_sent", { to: "user", has_room: !!roomId });
    return ref.id;
  }

  async function watchInvites(onInvite) {
    const env = await waitEnv();
    const { db, uid, collection, onSnapshot, query, where } = env;

    const q = query(collection(db, "invites"), where("toUid", "==", uid));
    return onSnapshot(q, (snap) => {
      const invites = [];
      snap.forEach((d) => invites.push({ id: d.id, ...d.data() }));
      onInvite(invites);
    });
  }

  window.firebaseEnv = window.firebaseEnv || {};
  window.firebaseEnv.setProfile = setProfile;
  window.firebaseEnv.sendInvite = sendInvite;
  window.firebaseEnv.watchInvites = watchInvites;

  console.log("[friends] ready");
})();
