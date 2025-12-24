// ui/analytics.js
// Exposes firebaseEnv.analyticsLog(name, params)

(function () {
  const b = window.__firebaseBindings;
  if (!b || !b.app) return;

  // Lazy import analytics (works with your gstatic modular setup)
  let _logEvent = null;
  let _analytics = null;

  async function ensureAnalytics() {
    if (_analytics && _logEvent) return;
    const mod = await import("https://www.gstatic.com/firebasejs/10.7.1/firebase-analytics.js");
    _analytics = mod.getAnalytics(b.app);
    _logEvent = mod.logEvent;
  }

  async function analyticsLog(name, params = {}) {
    try {
      await ensureAnalytics();
      _logEvent(_analytics, name, params);
    } catch (e) {
      console.warn("[analytics] log failed", e);
    }
  }

  // attach
  window.firebaseEnv = window.firebaseEnv || {};
  window.firebaseEnv.analyticsLog = analyticsLog;

  console.log("[analytics] ready");
})();
