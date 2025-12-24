// ui/analytics.js
// Robust bridge: always ensures firebaseEnv.logEvent exists (even if firebaseEnv gets replaced)

(function () {
  function sleep(ms) {
    return new Promise((r) => setTimeout(r, ms));
  }

  async function waitReady(timeoutMs = 6000) {
    const start = Date.now();
    while (true) {
      if (window.firebaseEnv && window.__firebaseBindings) return true;
      if (Date.now() - start > timeoutMs) return false;
      await sleep(30);
    }
  }

  function safeParams(env, extra) {
    const p = Object.assign({}, extra || {});
    if (env && env.uid) p.uid = env.uid;
    if (env && env.roomId) p.room_id = env.roomId;
    if (env && env.role) p.role = env.role;
    return p;
  }

  function attach(env) {
    if (!env) return;

    const bindings = window.__firebaseBindings;

    // Always provide no-op functions so UI never sees "undefined"
    env._analyticsLog = (name, params) => {};
    env.logEvent = (name) => {};
    env.logEventWith = (name, params) => {};

    if (bindings && typeof bindings.analyticsLogEvent === "function") {
      env._analyticsLog = (name, params) =>
        bindings.analyticsLogEvent(name, safeParams(env, params));

      env.logEvent = (name) => env._analyticsLog(name, {});
      env.logEventWith = (name, params) => env._analyticsLog(name, params || {});
    }
  }

  (async function () {
    await waitReady();

    // attach once now
    attach(window.firebaseEnv);

    // re-attach on every env change (in case firebaseEnv object got replaced)
    window.addEventListener("firebaseEnvChanged", () => {
      attach(window.firebaseEnv);
    });

    console.log("[analytics] bridge ready (robust)");
  })();
})();
