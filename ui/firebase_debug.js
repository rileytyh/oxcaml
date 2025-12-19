// ui/firebase_debug.js
(async function () {
  const b = window.__firebaseBindings;
  if (!b || !b.auth || !b.db) {
    console.error("[firebase_debug] Missing window.__firebaseBindings. Check index.html module script.");
    return;
  }

  try {
    // reuse existing user if already signed in
    let user = b.auth.currentUser;
    if (!user) {
      const cred = await b.signInAnonymously(b.auth);
      user = cred.user;
    }

    window.firebaseEnv = {
      ready: true,
      uid: user.uid,
      db: b.db,

      // firestore fns
      collection: b.collection,
      doc: b.doc,
      getDocs: b.getDocs,
      getDoc: b.getDoc,
      addDoc: b.addDoc,
      setDoc: b.setDoc,
      updateDoc: b.updateDoc,
      onSnapshot: b.onSnapshot,
      serverTimestamp: b.serverTimestamp,
      query: b.query,
      where: b.where,
      limit: b.limit,

      // runtime fields set by quickmatch.js
      roomId: null,
      role: null,
      sendState: null,

      // used to prevent echo loop after applying remote
      _suppressSendUntil: 0
    };

    console.log("[firebase_debug] signed in anon uid =", window.firebaseEnv.uid);
  } catch (e) {
    console.error("[firebase_debug] sign-in failed:", e);
  }
})();
