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

// Active nav link with random neon background
(function () {
  var neons = ['#FF0080', '#00FFAA', '#AAFF00', '#00EEFF', '#FF6600', '#CC00FF', '#FFEE00', '#FF3366'];

  function markActive() {
    var path = window.location.pathname;
    var color = neons[Math.floor(Math.random() * neons.length)];
    var links = document.querySelectorAll('nav ul li a');
    links.forEach(function (a) {
      var href = a.getAttribute('href') || '';
      var target = href.replace(/^(\.\.\/)+/, '');
      var page = path.split('/').pop() || 'index.html';
      var isProject = path.includes('/projects/');

      if (isProject && target === 'portfolio.html') {
        a.classList.add('active');
        a.style.background = color;
      } else if (!isProject && page === target) {
        a.classList.add('active');
        a.style.background = color;
      } else if (!isProject && page === '' && target === 'index.html') {
        a.classList.add('active');
        a.style.background = color;
      }
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', markActive);
  } else {
    markActive();
  }
})();
