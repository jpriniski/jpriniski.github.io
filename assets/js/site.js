// Mobile nav hamburger
(function () {
  function initNav() {
    var header = document.querySelector('header');
    if (!header) return;
    var navInner = header.querySelector('.nav-inner');
    if (!navInner) return;
    var nav = header.querySelector('nav');
    nav.id = 'site-navigation';
    nav.setAttribute('aria-label', 'Main navigation');

    var btn = document.createElement('button');
    btn.className = 'nav-toggle';
    btn.setAttribute('aria-label', 'Toggle navigation');
    btn.setAttribute('aria-expanded', 'false');
    btn.setAttribute('aria-controls', nav.id);
    btn.textContent = '☰';
    navInner.appendChild(btn);
    header.classList.add('nav-ready');

    document.addEventListener('keydown', function (event) {
      if (event.key === 'Escape' && header.classList.contains('nav-open')) {
        header.classList.remove('nav-open');
        btn.setAttribute('aria-expanded', 'false');
        btn.textContent = '☰';
        btn.focus();
      }
    });

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

// Use the device share menu, with a copyable link for other browsers.
(function () {
  function initShare() {
    var button = document.querySelector('[data-share-url]');
    if (!button) return;
    var label = button.querySelector('[data-share-label]');
    var status = document.querySelector('[data-share-status]');
    var fallback = document.querySelector('[data-share-fallback]');
    var input = fallback.querySelector('input');
    var url = button.getAttribute('data-share-url');
    var resetLabel;
    button.hidden = false;

    function manualCopy() {
      fallback.hidden = false;
      input.value = url;
      input.focus();
      input.select();
      status.textContent = 'Select and copy the website link to share it.';
    }

    button.addEventListener('click', async function () {
      if (button.disabled) return;
      button.disabled = true;
      status.textContent = '';
      fallback.hidden = true;
      try {
        if (navigator.share) {
          try {
            await navigator.share({ title: 'Hunter Priniski, PhD', url: url });
            return;
          } catch (error) {
            if (error.name === 'AbortError') return;
          }
        }
        if (navigator.clipboard && navigator.clipboard.writeText) {
          try {
            await navigator.clipboard.writeText(url);
            label.textContent = 'Copied!';
            status.textContent = 'Website link copied. Paste it into a message or post.';
            clearTimeout(resetLabel);
            resetLabel = setTimeout(function () { label.textContent = 'Share'; }, 2500);
            return;
          } catch (error) { /* The browser may block clipboard access. */ }
        }
        manualCopy();
      } finally {
        button.disabled = false;
      }
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initShare);
  } else {
    initShare();
  }
})();

// Filter the original research grid, including links from homepage themes.
(function () {
  function initResearchFilters() {
    var filters = document.querySelector('[data-research-filters]');
    if (!filters) return;
    var grid = document.getElementById('research-projects');
    var status = document.querySelector('[data-research-filter-status]');
    var buttons = Array.from(filters.querySelectorAll('[data-research-filter]'));
    var cards = Array.from(grid.querySelectorAll('[data-research-themes]'));
    var topicButtons = Array.from(grid.querySelectorAll('[data-research-topic]'));
    var topicGroups = Array.from(grid.querySelectorAll('.card-topics'));
    var controls = buttons.concat(topicButtons);
    var themes = buttons.map(function (button) { return button.getAttribute('data-research-filter'); });
    var aliases = {
      'human-ai-systems': 'narrative-agency',
      'collective-narratives': 'narrative-agency',
      'collective-dynamics': 'narrative-agency',
      'individual-collective-narratives': 'narrative-agency',
      'belief-change': 'social-media-dynamics'
    };

    function readFilter() {
      var query = new URLSearchParams(window.location.search).get('theme');
      query = aliases[query] || query;
      if (themes.includes(query)) return query;
      // Previously shared theme anchors still open the matching filter.
      var hash = window.location.hash.slice(1);
      hash = aliases[hash] || hash;
      return themes.includes(hash) ? hash : 'all';
    }

    function applyFilter(theme, updateUrl) {
      var visible = 0;
      cards.forEach(function (card) {
        var matches = theme === 'all' || card.getAttribute('data-research-themes').split(/\s+/).includes(theme);
        card.hidden = !matches;
        if (matches) visible += 1;
      });
      controls.forEach(function (button) {
        var value = button.getAttribute('data-research-filter') || button.getAttribute('data-research-topic');
        button.setAttribute('aria-pressed', value === theme ? 'true' : 'false');
      });
      status.textContent = theme === 'all' ? 'Showing all ' + cards.length + ' projects' : 'Showing ' + visible + ' of ' + cards.length + ' projects';

      if (updateUrl) {
        var url = new URL(window.location.href);
        if (theme === 'all') url.searchParams.delete('theme');
        else url.searchParams.set('theme', theme);
        if (themes.includes(url.hash.slice(1)) || themes.includes(aliases[url.hash.slice(1)])) url.hash = '';
        if (url.href !== window.location.href) window.history.pushState(null, '', url.href);
      }
    }

    controls.forEach(function (button) {
      button.addEventListener('click', function () {
        applyFilter(button.getAttribute('data-research-filter') || button.getAttribute('data-research-topic'), true);
      });
    });
    window.addEventListener('popstate', function () { applyFilter(readFilter(), false); });
    window.addEventListener('hashchange', function () { applyFilter(readFilter(), false); });
    applyFilter(readFilter(), false);
    filters.hidden = false;
    status.hidden = false;
    topicGroups.forEach(function (group) { group.hidden = false; });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initResearchFilters);
  } else {
    initResearchFilters();
  }
})();

// Prepare digest signup requests through the project's existing email route.
(function () {
  function initDigestSignup() {
    document.querySelectorAll('form[data-digest-signup]').forEach(function (form) {
      var name = form.elements.namedItem('name');
      var email = form.elements.namedItem('email');
      name.addEventListener('input', function () { name.setCustomValidity(''); });
      form.addEventListener('submit', function (event) {
        event.preventDefault();
        name.setCustomValidity(name.value.trim() ? '' : 'Please enter your name.');
        if (!form.reportValidity()) return;
        var body = 'Please add me to the slop digest mailing list.\r\n\r\n'
          + 'Name: ' + name.value.trim() + '\r\n'
          + 'Email: ' + email.value.trim();
        window.location.href = 'mailto:' + form.getAttribute('data-digest-signup')
          + '?subject=' + encodeURIComponent('Slop digest signup')
          + '&body=' + encodeURIComponent(body);
      });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initDigestSignup);
  } else {
    initDigestSignup();
  }
})();

// Keep game input in a separate player while the page stays in place.
(function () {
  function initGameViewer() {
    var links = document.querySelectorAll('.game-launch');
    if (!links.length || typeof HTMLDialogElement === 'undefined') return;

    var dialog = document.createElement('dialog');
    dialog.className = 'game-viewer';
    dialog.id = 'game-viewer';
    dialog.setAttribute('aria-labelledby', 'game-viewer-title');
    var heading = document.createElement('div');
    heading.className = 'game-viewer-heading';
    var title = document.createElement('h2');
    title.id = 'game-viewer-title';
    var close = document.createElement('button');
    close.className = 'game-viewer-close';
    close.type = 'button';
    close.setAttribute('aria-label', 'Close game');
    close.textContent = '×';
    heading.appendChild(title);
    heading.appendChild(close);
    var stage = document.createElement('div');
    stage.className = 'game-viewer-stage';
    var frame = document.createElement('iframe');
    frame.setAttribute('allow', 'fullscreen');
    frame.setAttribute('allowfullscreen', '');
    stage.appendChild(frame);
    var controls = document.createElement('p');
    controls.className = 'game-viewer-controls';
    controls.textContent = 'WASD / arrows: move · E: interact · Touch: swipe and tap · ×: close';
    dialog.appendChild(heading);
    dialog.appendChild(stage);
    dialog.appendChild(controls);
    document.body.appendChild(dialog);

    var trigger;
    var saved;
    frame.addEventListener('load', function () {
      if (dialog.open && frame.hasAttribute('src')) frame.focus();
    });
    links.forEach(function (link) {
      link.setAttribute('aria-haspopup', 'dialog');
      link.setAttribute('aria-controls', dialog.id);
      link.addEventListener('click', function (event) {
        if (event.ctrlKey || event.metaKey || event.shiftKey || event.altKey) return;
        event.preventDefault();
        trigger = link;
        saved = {
          x: window.scrollX, y: window.scrollY,
          position: document.body.style.position,
          top: document.body.style.top,
          left: document.body.style.left,
          width: document.body.style.width,
          overflow: document.body.style.overflow,
          htmlOverflow: document.documentElement.style.overflow
        };
        title.textContent = link.getAttribute('data-game-title') || 'Game';
        frame.title = 'Play ' + title.textContent;
        document.body.style.position = 'fixed';
        document.body.style.top = -saved.y + 'px';
        document.body.style.left = -saved.x + 'px';
        document.body.style.width = '100%';
        document.body.style.overflow = 'hidden';
        document.documentElement.style.overflow = 'hidden';
        dialog.showModal();
        close.focus();
        frame.src = link.href;
      });
    });
    close.addEventListener('click', function () { dialog.close(); });
    dialog.addEventListener('click', function (event) {
      var bounds = dialog.getBoundingClientRect();
      if (event.target === dialog && (event.clientX < bounds.left || event.clientX > bounds.right || event.clientY < bounds.top || event.clientY > bounds.bottom)) {
        dialog.close();
      }
    });
    dialog.addEventListener('keydown', function (event) {
      // Events inside the game iframe stay in its own document.
      if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'PageUp', 'PageDown', 'Home', 'End'].includes(event.key)) event.preventDefault();
    });
    dialog.addEventListener('close', function () {
      frame.removeAttribute('src');
      if (!saved) return;
      document.body.style.position = saved.position;
      document.body.style.top = saved.top;
      document.body.style.left = saved.left;
      document.body.style.width = saved.width;
      document.body.style.overflow = saved.overflow;
      document.documentElement.style.overflow = saved.htmlOverflow;
      var scrollBehavior = document.documentElement.style.scrollBehavior;
      document.documentElement.style.scrollBehavior = 'auto';
      window.scrollTo(saved.x, saved.y);
      document.documentElement.style.scrollBehavior = scrollBehavior;
      if (trigger) trigger.focus({ preventScroll: true });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initGameViewer);
  } else {
    initGameViewer();
  }
})();

// Give the active section the site's original changing neon highlight.
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
      var projectSection = ['digital-community-gardens.html', 'vitality-az.html', 'faithscape.html', 'mediaslop.html'].includes(page)
        ? 'new-media.html' : 'portfolio.html';

      if (isProject && target === projectSection) {
        a.classList.add('active');
        a.style.background = color;
        a.setAttribute('aria-current', 'page');
      } else if (!isProject && page === target) {
        a.classList.add('active');
        a.style.background = color;
        a.setAttribute('aria-current', 'page');
      } else if (!isProject && page === '' && target === 'index.html') {
        a.classList.add('active');
        a.style.background = color;
        a.setAttribute('aria-current', 'page');
      }
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', markActive);
  } else {
    markActive();
  }
})();

// Open research figures without navigating away from the paper.
(function () {
  function initFigureViewer() {
    var links = document.querySelectorAll('.figure-link');
    if (!links.length || typeof HTMLDialogElement === 'undefined') return;

    var dialog = document.createElement('dialog');
    dialog.className = 'figure-viewer';
    dialog.setAttribute('aria-label', 'Figure');
    var close = document.createElement('button');
    close.className = 'figure-viewer-close';
    close.type = 'button';
    close.setAttribute('aria-label', 'Close figure');
    close.textContent = '×';
    var img = document.createElement('img');
    dialog.appendChild(close);
    dialog.appendChild(img);
    document.body.appendChild(dialog);
    var trigger;
    var previousOverflow;

    links.forEach(function (link) {
      link.setAttribute('aria-haspopup', 'dialog');
      link.addEventListener('click', function (event) {
        if (event.ctrlKey || event.metaKey || event.shiftKey || event.altKey) return;
        event.preventDefault();
        trigger = link;
        img.src = link.href;
        var original = link.querySelector('img');
        img.alt = original ? original.alt : '';
        previousOverflow = document.body.style.overflow;
        dialog.showModal();
        document.body.style.overflow = 'hidden';
        close.focus();
      });
    });
    close.addEventListener('click', function () { dialog.close(); });
    dialog.addEventListener('click', function (event) {
      if (event.target === dialog) dialog.close();
    });
    dialog.addEventListener('close', function () {
      document.body.style.overflow = previousOverflow;
      if (trigger) trigger.focus({ preventScroll: true });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initFigureViewer);
  } else {
    initFigureViewer();
  }
})();
