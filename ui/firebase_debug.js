// ui/firebase_debug.js
// - Does NOT auto sign-in.
// - Exposes window.firebaseEnv with signIn() on demand.
// - Adds Notification permission helpers.
// - Keeps env stable across reload and emits firebaseEnvChanged.

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

  // If someone overwrote firebaseEnv earlier, keep fields but patch missing APIs
  const env = window.firebaseEnv && typeof window.firebaseEnv === "object"
    ? window.firebaseEnv
    : {};

  // core
  env.ready = env.ready || false;
  env.uid = env.uid || null;
  env.db = b.db;
  env.auth = b.auth;

  // firestore fns
  env.collection = b.collection;
  env.doc = b.doc;
  env.getDocs = b.getDocs;
  env.getDoc = b.getDoc;
  env.addDoc = b.addDoc;
  env.setDoc = b.setDoc;
  env.updateDoc = b.updateDoc;
  env.onSnapshot = b.onSnapshot;
  env.serverTimestamp = b.serverTimestamp;
  env.query = b.query;
  env.where = b.where;
  env.limit = b.limit;

  // runtime fields set by quickmatch.js
  env.roomId = env.roomId || null;
  env.role = env.role || null;
  env.status = env.status || "signed_out"; // signed_out | signing_in | signed_in | waiting | matched | error
  env.sendState = env.sendState || null;

  // conservative default (prevents accidental reseed)
  if (typeof env.roomHasState !== "boolean") env.roomHasState = true;

  // prevent echo loop after applying remote
  env._suppressSendUntil = env._suppressSendUntil || 0;

  // analytics placeholders (analytics.js will overwrite with real impl if available)
  if (typeof env.logEvent !== "function") env.logEvent = () => {};
  if (typeof env.logEventWith !== "function") env.logEventWith = () => {};
  if (typeof env._analyticsLog !== "function") env._analyticsLog = () => {};

  // notification flags
  if (typeof env.notificationsEnabled !== "boolean") env.notificationsEnabled = false;

  env.shortUid = () => shortId(env.uid, 6);

  // Notification permission request
  env.requestNotificationPermission = async () => {
    try {
      if (!("Notification" in window)) {
        console.warn("[notify] Notification API not supported");
        return "unsupported";
      }

      let perm = Notification.permission;
      if (perm === "granted") {
        env.notificationsEnabled = true;
        emitChanged();
        return perm;
      }

      perm = await Notification.requestPermission();
      env.notificationsEnabled = (perm === "granted");
      emitChanged();
      console.log("[notify] permission =", perm);
      return perm;
    } catch (e) {
      console.warn("[notify] requestPermission failed:", e);
      return "error";
    }
  };

  // fire a notification (safe)
  env.notify = (title, options) => {
    try {
      if (!("Notification" in window)) return false;
      if (Notification.permission !== "granted") return false;
      if (!env.notificationsEnabled) return false;
      new Notification(title, options || {});
      return true;
    } catch (e) {
      console.warn("[notify] failed:", e);
      return false;
    }
  };

  // Sign in on demand (anonymous/guest)
  env.signIn = async () => {
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
      env.logEvent("sign_in");
      emitChanged();

      console.log("[firebase_debug] signed in anon uid =", env.uid);
      return env.uid;
    } catch (e) {
      console.error("[firebase_debug] sign-in failed:", e);
      env.status = "error";
      emitChanged();
      throw e;
    }
  };

  window.firebaseEnv = env;
  emitChanged();

  // stash acquisition params on env so analytics.js can read env.acq
  try {
    window.firebaseEnv.acq = window.__acq || window._acq || null;
  } catch {}

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
