// ui/messaging.js
// Requests notification permission, registers SW, stores FCM token in Firestore

(function () {
  async function ensure() {
    const b = window.__firebaseBindings;
    const env = window.firebaseEnv;
    if (!b || !b.app || !env) throw new Error("bindings/env missing");

    const mod = await import("https://www.gstatic.com/firebasejs/10.7.1/firebase-messaging.js");
    const { getMessaging, getToken } = mod;

    // IMPORTANT: register SW (scope must match your hosting path)
    const reg = await navigator.serviceWorker.register("./firebase-messaging-sw.js");

    const messaging = getMessaging(b.app);

    // TODO: put your Web Push certificate key (VAPID key) here
    const vapidKey = "YOUR_VAPID_KEY";

    const token = await getToken(messaging, { vapidKey, serviceWorkerRegistration: reg });
    if (!token) throw new Error("No FCM token (permission?)");

    // store token
    const { db, uid, doc, setDoc, serverTimestamp } = env;
    await setDoc(doc(db, "users", uid, "fcmTokens", token), {
      createdAt: serverTimestamp(),
      ua: navigator.userAgent
    });

    env.analyticsLog && env.analyticsLog("push_token_saved", { ok: 1 });
    return token;
  }

  window.firebaseEnv = window.firebaseEnv || {};
  window.firebaseEnv.enablePush = async () => {
    const perm = await Notification.requestPermission();
    if (perm !== "granted") throw new Error("Notification permission denied");
    return ensure();
  };

  console.log("[messaging] ready");
})();
