// Mediaslop popup — appears after 5 seconds
(function () {
  function buildPopup() {
    var popup = document.createElement('div');
    popup.id = 'ms-popup';
    popup.innerHTML = [
      '<div id="ms-popup-inner">',
      '  <div id="ms-popup-header">',
      '    <span id="ms-popup-label">// MEDIASLOP.COM</span>',
      '    <button id="ms-popup-close" aria-label="Close">&#x2715;</button>',
      '  </div>',
      '  <p id="ms-popup-body">Check out the arcade and Peepshow</p>',
      '  <a id="ms-popup-cta" href="https://mediaslop.com" target="_blank" rel="noopener">Enter &rarr;</a>',
      '</div>'
    ].join('');

    var style = document.createElement('style');
    style.textContent = [
      '#ms-popup {',
      '  position: fixed;',
      '  bottom: 32px;',
      '  right: 32px;',
      '  z-index: 99999;',
      '  opacity: 0;',
      '  transform: translateY(14px);',
      '  transition: opacity 0.35s ease, transform 0.35s ease;',
      '  pointer-events: none;',
      '}',
      '#ms-popup.ms-visible {',
      '  opacity: 1;',
      '  transform: translateY(0);',
      '  pointer-events: auto;',
      '}',
      '#ms-popup-inner {',
      '  background: #f5e800;',
      '  border: 3px solid #c8bf00;',
      '  box-shadow: 6px 6px 0 #080808, 0 8px 40px rgba(0,0,0,0.7);',
      '  padding: 24px 28px 26px;',
      '  width: 340px;',
      '  font-family: "IBM Plex Mono", "Courier New", monospace;',
      '}',
      '#ms-popup-header {',
      '  display: flex;',
      '  justify-content: space-between;',
      '  align-items: center;',
      '  margin-bottom: 14px;',
      '}',
      '#ms-popup-label {',
      '  font-size: 13px;',
      '  letter-spacing: 0.2em;',
      '  text-transform: uppercase;',
      '  color: #080808;',
      '  font-weight: 700;',
      '}',
      '#ms-popup-close {',
      '  background: none;',
      '  border: 2px solid #080808;',
      '  color: #080808;',
      '  cursor: pointer;',
      '  font-family: inherit;',
      '  font-size: 13px;',
      '  font-weight: 700;',
      '  padding: 2px 8px;',
      '  line-height: 1;',
      '  transition: background 0.15s, color 0.15s;',
      '}',
      '#ms-popup-close:hover {',
      '  background: #080808;',
      '  color: #f5e800;',
      '}',
      '#ms-popup-body {',
      '  font-size: 16px;',
      '  color: #080808;',
      '  line-height: 1.6;',
      '  margin-bottom: 22px;',
      '  font-weight: 500;',
      '}',
      '#ms-popup-cta {',
      '  display: inline-block;',
      '  font-size: 13px;',
      '  letter-spacing: 0.18em;',
      '  text-transform: uppercase;',
      '  font-weight: 700;',
      '  padding: 12px 24px;',
      '  background: #080808;',
      '  color: #f5e800;',
      '  text-decoration: none;',
      '  border: 2px solid #080808;',
      '  transition: background 0.15s, color 0.15s;',
      '}',
      '#ms-popup-cta:hover {',
      '  background: #1a1a1a;',
      '  color: #ffffff;',
      '}',
      '@media (max-width: 540px) {',
      '  #ms-popup { left: 12px; right: 12px; bottom: 16px; }',
      '  #ms-popup-inner { width: auto; }',
      '}'
    ].join('\n');

    document.head.appendChild(style);
    document.body.appendChild(popup);

    // Show after 5 seconds
    setTimeout(function () {
      popup.classList.add('ms-visible');
    }, 5000);

    // Dismiss on close button
    document.getElementById('ms-popup-close').addEventListener('click', function () {
      popup.classList.remove('ms-visible');
      // Don't show again this session
      setTimeout(function () { popup.remove(); }, 400);
    });
  }

  // Only run on the home page
  var path = window.location.pathname;
  var isHome = path === '/' || path.endsWith('/index.html') || path.endsWith('/priniski.com/');
  if (!isHome) return;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', buildPopup);
  } else {
    buildPopup();
  }
})();

// Mobile nav hamburger
(function () {
  function initNav() {
    var header = document.querySelector('header');
    if (!header) return;
    var navInner = header.querySelector('.nav-inner');
    if (!navInner) return;

    var btn = document.createElement('button');
    btn.className = 'nav-toggle';
    btn.setAttribute('aria-label', 'Toggle navigation');
    btn.setAttribute('aria-expanded', 'false');
    btn.textContent = '☰';
    navInner.appendChild(btn);

    btn.addEventListener('click', function () {
      var open = header.classList.toggle('nav-open');
      btn.setAttribute('aria-expanded', open ? 'true' : 'false');
      btn.textContent = open ? '✕' : '☰';
    });

    header.querySelectorAll('nav a').forEach(function (a) {
      a.addEventListener('click', function () {
        header.classList.remove('nav-open');
        btn.setAttribute('aria-expanded', 'false');
        btn.textContent = '☰';
      });
    });

    document.addEventListener('click', function (e) {
      if (!header.contains(e.target)) {
        header.classList.remove('nav-open');
        btn.setAttribute('aria-expanded', 'false');
        btn.textContent = '☰';
      }
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initNav);
  } else {
    initNav();
  }
})();
