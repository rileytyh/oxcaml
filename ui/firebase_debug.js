// ui/firebase_debug.js
// - Does NOT auto sign-in.
// - Exposes window.firebaseEnv with signIn() that performs anonymous sign-in on demand.
// - Also listens to auth state changes so refresh/reload stays consistent.

(function () {
  const b = window.__firebaseBindings;
  if (!b || !b.auth || !b.db) {
    console.error(
      "[firebase_debug] Missing window.__firebaseBindings. Check index.html module script."
    );
    return;
  }

  function emitChanged() {
    try {
      window.dispatchEvent(
        new CustomEvent("firebaseEnvChanged", { detail: window.firebaseEnv })
      );
    } catch {
      // ignore
    }
  }

  function shortId(x, n = 6) {
    if (!x) return "—";
    if (x.length <= n) return x;
    return x.slice(0, n) + "…" + x.slice(-2);
  }

  const env = {
    // core
    ready: false,
    uid: null,
    db: b.db,
    auth: b.auth,

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
    status: "signed_out", // signed_out | signing_in | signed_in | waiting | matched | error
    sendState: null,

    // conservative default (prevents accidental reseed)
    roomHasState: true,

    // prevent echo loop after applying remote
    _suppressSendUntil: 0,

    // helper
    shortUid: () => shortId(env.uid, 6),

    // Sign in on demand (anonymous/guest)
    signIn: async () => {
      try {
        env.status = "signing_in";
        emitChanged();

        let user = b.auth.currentUser;
        if (!user) {
          const cred = await b.signInAnonymously(b.auth);
          user = cred.user;
        }

        env.uid = user.uid;
        env.ready = true;
        env.status = "signed_in";
        emitChanged();

        console.log("[firebase_debug] signed in anon uid =", env.uid);
        return env.uid;
      } catch (e) {
        console.error("[firebase_debug] sign-in failed:", e);
        env.status = "error";
        emitChanged();
        throw e;
      }
    },
  };

  window.firebaseEnv = env;
  emitChanged();

  // Keep env in sync on refresh/reload if already signed in
  try {
    if (typeof b.onAuthStateChanged === "function") {
      b.onAuthStateChanged(b.auth, (user) => {
        if (user) {
          env.uid = user.uid;
          env.ready = true;
          if (env.status === "signed_out") env.status = "signed_in";
        } else {
          env.uid = null;
          env.ready = false;
          env.status = "signed_out";
          env.roomId = null;
          env.role = null;
        }
        emitChanged();
      });
    }
  } catch (e) {
    console.warn("[firebase_debug] onAuthStateChanged hook failed:", e);
  }

  console.log("[firebase_debug] ready. (Not signed in yet)");
})();
