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

  function ensureBanner() {
    let el = document.getElementById("hw13-ad-banner");
    if (el) return el;

    el = document.createElement("div");
    el.id = "hw13-ad-banner";
    el.style.position = "fixed";
    el.style.left = "0";
    el.style.right = "0";
    el.style.bottom = "0";
    el.style.zIndex = "9999";
    el.style.display = "flex";
    el.style.alignItems = "center";
    el.style.justifyContent = "space-between";
    el.style.gap = "12px";
    el.style.padding = "12px 14px";
    el.style.background = "rgba(0,0,0,0.80)";
    el.style.borderTop = "1px solid rgba(255,255,255,0.10)";
    el.style.color = "#eaeaea";
    el.style.backdropFilter = "blur(6px)";

    const left = document.createElement("div");
    left.innerHTML = `<div style="font-weight:800">Ad</div>
      <div style="opacity:.85;font-size:12px">This is a demo ad banner (HW13). Remove it by purchasing “Remove Ads”.</div>`;

    const right = document.createElement("div");
    right.style.display = "flex";
    right.style.gap = "10px";

    const buy = document.createElement("button");
    buy.id = "hw13-buy";
    buy.textContent = "Remove Ads ($2)";
    buy.style.padding = "8px 12px";
    buy.style.borderRadius = "12px";
    buy.style.border = "1px solid rgba(255,255,255,0.14)";
    buy.style.background = "rgba(255,255,255,0.06)";
    buy.style.color = "#fff";
    buy.style.fontWeight = "800";
    buy.style.cursor = "pointer";

    const hide = document.createElement("button");
    hide.textContent = "Hide";
    hide.style.padding = "8px 12px";
    hide.style.borderRadius = "12px";
    hide.style.border = "1px solid rgba(255,255,255,0.14)";
    hide.style.background = "rgba(255,255,255,0.04)";
    hide.style.color = "#fff";
    hide.style.cursor = "pointer";

    hide.onclick = () => {
      el.style.display = "none";
    };

    right.appendChild(buy);
    right.appendChild(hide);

    el.appendChild(left);
    el.appendChild(right);

    document.body.appendChild(el);
    return el;
  }

  function setBannerVisible(visible) {
    const el = ensureBanner();
    el.style.display = visible ? "flex" : "none";
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

  function goToStripeCheckout(env) {
    // 强制要求先有 uid，不然回调后无法把购买写入 Firestore
    if (!env.uid) {
      alert("Please click “Sign in (Guest)” first, then purchase again.");
      try {
        env.logEventWith &&
          env.logEventWith("purchase_blocked_no_uid", { product: "remove_ads" });
      } catch {}
      return;
    }

    try {
      env.logEventWith &&
        env.logEventWith("purchase_start", { product: "remove_ads", via: "stripe_link" });
    } catch {}

    window.location.href = STRIPE_PAYMENT_LINK;
  }

  async function main() {
    const env = await waitEnv();

    // Always ensure banner exists (we'll decide show/hide later)
    const banner = ensureBanner();

    // Buy button -> Stripe link
    banner.querySelector("#hw13-buy").onclick = () => goToStripeCheckout(env);

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
