// Narrow-viewport helpers: a <select> that mirrors card tab links, and a
// plotly / htmlwidgets resize when a tab is shown or a sidebar opens.
// Breakpoint matches sc-responsive.css (1200px). No extra Shiny messages.
(function () {
  var BP = 1200;

  function resizePlots() {
    try {
      if (window.Plotly && Plotly.Plots && typeof Plotly.Plots.resize === "function") {
        document.querySelectorAll(".js-plotly-plot").forEach(function (el) {
          try { Plotly.Plots.resize(el); } catch (err) {}
        });
      }
    } catch (err) {}
  }

  function requestLayout() {
    resizePlots();
    try { window.dispatchEvent(new Event("resize")); } catch (err) {}
    // A second pass after CSS transitions and plotly relayout.
    window.setTimeout(resizePlots, 80);
    window.setTimeout(resizePlots, 280);
  }

  function tabLinks(nav) {
    return Array.from(nav.querySelectorAll(":scope > .nav-item > .nav-link, :scope > .nav-link"));
  }

  function syncSelect(sel, links) {
    links.forEach(function (a, i) {
      if (a.classList.contains("active") || a.getAttribute("aria-selected") === "true") {
        sel.value = String(i);
      }
    });
  }

  function enhanceNav(nav) {
    if (!nav || nav.dataset.scTabSelect === "1") return;
    if (nav.closest(".navbar")) return;
    if (!(nav.classList.contains("nav-tabs") || nav.classList.contains("nav-underline"))) return;
    var links = tabLinks(nav);
    if (links.length < 2) return;
    nav.dataset.scTabSelect = "1";
    var sel = document.createElement("select");
    sel.className = "form-select sc-tab-select";
    sel.setAttribute("aria-label", "Choose a view");
    links.forEach(function (a, i) {
      var opt = document.createElement("option");
      opt.value = String(i);
      opt.textContent = (a.textContent || a.innerText || "").replace(/\s+/g, " ").trim();
      sel.appendChild(opt);
    });
    syncSelect(sel, links);
    sel.addEventListener("change", function () {
      var a = links[Number(sel.value)];
      if (a) a.click();
      requestLayout();
    });
    nav.addEventListener("shown.bs.tab", function () {
      syncSelect(sel, links);
      requestLayout();
    });
    var wrap = document.createElement("div");
    wrap.className = "sc-tab-select-wrap";
    wrap.appendChild(sel);
    nav.parentNode.insertBefore(wrap, nav);
  }

  function scan() {
    document.querySelectorAll(".card .nav-tabs, .card .nav-underline, .card-header .nav").forEach(enhanceNav);
  }

  function onSidebarChange(ev) {
    var t = ev && ev.target;
    if (!t) return;
    if (t.classList && t.classList.contains("bslib-sidebar-layout")) {
      requestLayout();
      return;
    }
    if (t.closest && t.closest(".bslib-sidebar-layout")) requestLayout();
  }

  var wasNarrow = null;

  function setCollapsed(el, wantCollapsed) {
    var isCollapsed = el.classList.contains("sidebar-collapsed");
    if (isCollapsed === wantCollapsed) return;
    var toggle = el.querySelector(":scope > .collapse-toggle");
    if (toggle) toggle.click();
  }

  // Collapse every sidebar below 1200px, restore them above it. Only acts
  // when the viewport crosses the breakpoint, so a user can still open a
  // sidebar on a narrow screen without it being yanked shut on a tiny resize.
  function applySidebarBreakpoint(force) {
    var narrow = window.matchMedia("(max-width: 1199.98px)").matches;
    if (!force && wasNarrow === narrow) return;
    wasNarrow = narrow;
    document.querySelectorAll(".bslib-sidebar-layout").forEach(function (el) {
      setCollapsed(el, narrow);
    });
    requestLayout();
  }
  window.scApplyResponsiveLayout = function (force) {
    applySidebarBreakpoint(!!force);
  };

  function start() {
    scan();
    applySidebarBreakpoint(true);
    var mq = window.matchMedia("(max-width: 1199.98px)");
    if (mq.addEventListener) mq.addEventListener("change", function () { applySidebarBreakpoint(); });
    else if (mq.addListener) mq.addListener(function () { applySidebarBreakpoint(); });
    var obs = new MutationObserver(function () { scan(); });
    if (document.body) {
      obs.observe(document.body, { childList: true, subtree: true });
    }
    document.addEventListener("transitionend", onSidebarChange);
    document.addEventListener("click", function (ev) {
      if (ev.target && ev.target.closest && ev.target.closest(".bslib-sidebar-layout > .collapse-toggle")) {
        window.setTimeout(requestLayout, 50);
        window.setTimeout(requestLayout, 320);
      }
    });
    window.addEventListener("resize", function () { resizePlots(); });
    if (window.jQuery) {
      window.jQuery(document).on("shiny:connected shiny:value shown.bs.tab", function () {
        scan();
        window.setTimeout(resizePlots, 50);
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", start);
  } else {
    start();
  }
})();
