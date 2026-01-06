// ui/monetization.js
// HW13 Monetization: "Remove Ads" purchase flow WITH Stripe Payment Link (Test mode).
// - Shows an ad banner for users who haven't purchased
// - Clicking "Remove Ads" redirects to Stripe Payment Link
// - Stripe redirects back to your game with:
//      ?payment=success&product=remove_ads
// - On return, we write purchase to Firestore (purchases/{uid}) and hide the ad banner

(function () {
  // ✅ Put your Stripe Payment Link here (Test mode link)
  const STRIPE_PAYMENT_LINK = "https://buy.stripe.com/test_6oU9AT4w38eF2ZndjI83C00";

  function sleep(ms) {
    return new Promise((r) => setTimeout(r, ms));
  }

  async function waitEnv(timeoutMs = 12000) {
    const start = Date.now();
    while (true) {
      const env = window.firebaseEnv;
      const ok =
        env &&
        env.db &&
        env.doc &&
        env.getDoc &&
        env.setDoc &&
        env.updateDoc;
      if (ok) return env;
      if (Date.now() - start > timeoutMs)
        throw new Error("firebaseEnv not ready (timeout)");
      await sleep(50);
    }
  }

  async function waitUid(timeoutMs = 15000) {
    const env = await waitEnv();
    const start = Date.now();
    while (true) {
      if (env.uid) return env;
      if (Date.now() - start > timeoutMs)
        throw new Error("uid not ready (timeout)");
      await sleep(50);
    }
  }

  function qsGet(name) {
    try {
      return new URL(window.location.href).searchParams.get(name);
    } catch {
      return null;
    }
  }

  function cleanPaymentParams() {
    try {
      const u = new URL(window.location.href);
      u.searchParams.delete("payment");
      u.searchParams.delete("product");
      history.replaceState(
        {},
        "",
        u.pathname +
          (u.searchParams.toString() ? "?" + u.searchParams.toString() : "") +
          u.hash
      );
    } catch {}
  }

  function getBannerEl() {
    return document.getElementById("ad-banner-ocaml");
  }

  async function waitBannerEl(timeoutMs = 12000) {
    const start = Date.now();
    while (true) {
      const el = getBannerEl();
      if (el) return el;
      if (Date.now() - start > timeoutMs) return null;
      await sleep(50);
    }
  }

  function setBannerVisible(show) {
    const el = getBannerEl();
    if (!el) return;
    el.style.display = show ? "flex" : "none";
  }

  function installDelegation(env) {
    if (window.__hw13Delegated) return;
    window.__hw13Delegated = true;

    const goPay = (e) => {
      // 只响应 banner 内的点击
      const banner = e.target && e.target.closest && e.target.closest("#ad-banner-ocaml");
      if (!banner) return;

      // 点购买按钮 或 点击 banner 任意区域：去支付
      const buyBtn = e.target.closest('[data-action="remove-ads"]');
      const clickedInsideBanner = !!banner;
      if (buyBtn || clickedInsideBanner) {
        e.preventDefault();
        e.stopPropagation();
        if (!env || !env.uid) {
          alert('Please click "Sign in (Guest)" first, then purchase again.');
          return;
        }
        try {
          env.logEventWith && env.logEventWith("purchase_click", { product: "remove_ads", via: "banner" });
        } catch {}
        window.location.href = STRIPE_PAYMENT_LINK;
      }
    };

    // 用捕获阶段，避免被别的层拦截
    document.addEventListener("click", goPay, true);
  }

  async function loadPurchases(env) {
    const ref = env.doc(env.db, "purchases", env.uid);
    try {
      const snap = await env.getDoc(ref);
      return snap && snap.exists() ? snap.data() || {} : {};
    } catch (e) {
      console.warn("[hw13] loadPurchases failed:", e);
      return {};
    }
  }

  async function savePurchase(env, patch) {
    const ref = env.doc(env.db, "purchases", env.uid);
    try {
      await env.setDoc(ref, { ...patch, updated_at: Date.now() }, { merge: true });
      return true;
    } catch (e) {
      try {
        await env.updateDoc(ref, { ...patch, updated_at: Date.now() });
        return true;
      } catch (e2) {
        console.warn("[hw13] savePurchase failed:", e2);
        return false;
      }
    }
  }

  async function main() {
    const env = await waitEnv();

    // Robustly show banner even if OCaml mounts it later (first render race)
    setBannerVisible(true);

    // 1) Try wait a bit
    await waitBannerEl(12000);
    setBannerVisible(true);

    // 2) MutationObserver fallback: if banner is added later, force show
    try {
      if (!window.__hw13BannerObserver) {
        window.__hw13BannerObserver = true;
        const obs = new MutationObserver(() => {
          const el = getBannerEl();
          if (el) setBannerVisible(true);
        });
        obs.observe(document.documentElement, { childList: true, subtree: true });
      }
    } catch {}

    // Install event delegation (only once)
    installDelegation(env);

    // 3) Poll a few times on first load (extra safety)
    try {
      let tries = 0;
      const t = setInterval(() => {
        tries++;
        setBannerVisible(true);
        if (getBannerEl() || tries > 20) clearInterval(t); // ~10s
      }, 500);
    } catch {}

    // Wait for uid a bit; if user doesn't sign in, still show banner
    let gotUid = false;
    try {
      await waitUid(8000);
      gotUid = true;
    } catch {
      setBannerVisible(true);
    }

    // Handle Stripe redirect success
    const payment = qsGet("payment");
    const product = qsGet("product");

    if (gotUid && payment === "success" && product === "remove_ads") {
      const ok = await savePurchase(env, { ads_removed: true });
      try {
        env.logEventWith &&
          env.logEventWith("purchase_success", {
            product: "remove_ads",
            ok: ok ? "1" : "0",
            via: "stripe_link",
          });
      } catch {}
      cleanPaymentParams();
    }

    // Load purchases and apply UI
    if (gotUid) {
      const p = await loadPurchases(env);
      const adsRemoved = !!p.ads_removed;

      setBannerVisible(!adsRemoved);

      // expose for debugging
      env.purchases = p;
    }
  }

  main().catch((e) => console.error("[hw13] monetization init failed:", e));
})();
