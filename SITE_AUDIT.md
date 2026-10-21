# Site audit — rechecked 21 October 2026

The site can stay a small static website. Most excess storage is research source material and old assets; the useful refactor is shared page templates and project data. A framework rewrite would add complexity without resolving those issues.

Cleanup was completed on 21 October at your request. **85 files (186.5 MiB) were moved to `trash/`**, preserving their original paths and contents. `trash/manifest.json` records every move, size and SHA-256 checksum; `trash/README.md` explains restoration. Nothing was permanently deleted, pushed or deployed.

Immediately after cleanup, the active website was about **21.0 MiB**, excluding the archive. Page HTML, CSS and JavaScript was unchanged in that pass except the legacy redirect. Every archived file passed checksum verification, and local links and fragments pass after cleanup. No unreferenced assets remain outside `trash/`.

The root `.gitignore` excludes the archive from future commits; `.vercelignore` excludes it from Vercel uploads. The latter also excludes this maintenance report. See [Vercel’s ignore-file documentation](https://vercel.com/docs/deployments/vercel-ignore). Total disk usage still includes the recoverable archive.

The inventory below records the **pre-cleanup state**. Mobile and sharing changes from 20 October are preserved below as history.

## Inventory before cleanup

| Item | Finding | Recommended treatment |
| --- | --- | --- |
| Working files, excluding style backups and this report | 136 files, 207.4 MiB | Separate website output from working materials |
| Files reachable from the homepage | 59 files, 21.0 MiB | Starting point for a public asset manifest |
| `docs/` | 21 source files, 137.0 MiB; none linked by site pages | Keep in a private source archive outside public output |
| Assets referenced by no HTML page | 50 files, 35.7 MiB | Archive/prune after checking historical external links |
| Assets used only by an unlinked legacy page | 3 files, 11.1 MiB | Retire if that page becomes a redirect |
| Root research ZIP | 2.6 MiB, unlinked | Keep with research sources, exclude from output |
| Navigation and footer | 19 copies each with identical normalized content | Extract build-time partials |
| Core site styling/scripts | One shared stylesheet and one shared script across 19 pages | Preserve shared tokens and components |
| Exported notebook | Linked research artifact with its own export styling | Keep, separate from site templates |

A public build containing the currently linked files would be about **21 MiB rather than 207 MiB**, before image optimization and a possible legacy redirect. This is an artifact-size estimate, not a page-load speed estimate: visitors do not download every file in the workspace.

## Archived source materials

`docs/`, the root research ZIP, `.style-backups/`, `.DS_Store` files, and `assets/season_and_trim_chat_transcript.txt` were moved into `trash/`. These materials are not required to render the website. Research originals and the reversible style snapshot are preserved. The transcript’s contents were not read for this audit.

Large source archives include the knowledge-graph materials (~81 MiB) and Tucson game package (~28 MiB). The hosted Arizona Trail is embedded from Vitality AZ, so its local development ZIP is not a site dependency.

Exact duplicate files exist between `docs/` and public assets: the CV, narrative-agency paper, AI group-alignment paper, PNAS figure PDFs, and several COVID figures. Archive source copies; retain every public file still linked. Both the **CV and résumé PDFs are linked and should remain**.

### Archived assets with no references in any HTML page

The following files were moved to matching paths under `trash/`. The reference check includes legacy pages, active pages, stylesheet URLs, JavaScript-created elements, and SVG resource references. Their original paths are listed below. This establishes that the site does not use them; it cannot rule out historical external bookmarks.

- `assets/.DS_Store`
- `assets/icons/github.svg`
- `assets/icons/gmail.svg`
- `assets/icons/googlescholar.svg`
- `assets/icons/instagram.svg`
- `assets/icons/substack.svg`
- `assets/icons/tiktok.svg`
- `assets/icons/youtube.svg`
- `assets/images/Hashtags Large.png`
- `assets/images/Hashtags.png`
- `assets/images/MF.png`
- `assets/images/Network-Conditions.svg`
- `assets/images/RLViz.png`
- `assets/images/RLViz2.png`
- `assets/images/journal.pone.0263191.t001.PNG`
- `assets/images/new-media/mediaslop-logo.png`
- `assets/images/papers/arizona-trail-tucson.png`
- `assets/images/papers/digital-community-gardens.svg`
- `assets/images/papers/hybrid-page-3.png`
- `assets/images/papers/hybrid-page-6.png`
- `assets/images/papers/hybrid-page-7.png`
- `assets/images/papers/hybrid-page-9.png`
- `assets/images/papers/redlining-attitude-results.png`
- `assets/images/thumbs/Belief Dynamics.png`
- `assets/images/thumbs/Echochambers.png`
- `assets/images/thumbs/Hashtags Large.png`
- `assets/images/thumbs/MF embeddings.png`
- `assets/images/thumbs/MF.png`
- `assets/images/thumbs/RLViz.png`
- `assets/images/thumbs/RLViz2.png`
- `assets/images/thumbs/Social Learning.png`
- `assets/images/thumbs/journal.pone.0263191.g001 (1).PNG`
- `assets/images/thumbs/journal.pone.0263191.g008.PNG`
- `assets/images/thumbs/journal.pone.0263191.t001.PNG`
- `assets/images/thumbs/mediaslop.png`
- `assets/images/thumbs/newsage-press.png`
- `assets/images/thumbs/vitality-az.svg`
- `assets/pdfs/CogSci_2025.pdf`
- `assets/pdfs/Concentrated AI signals.pdf`
- `assets/pdfs/How Artificial Intelligence Shapes Human Narratives.pdf`
- `assets/pdfs/Online_Network_Topology.pdf`
- `assets/pdfs/PNAS Figure 2.pdf`
- `assets/pdfs/PNAS Figure 3.pdf`
- `assets/pdfs/PNAS Figure 4.pdf`
- `assets/pdfs/adams-election-topic.pdf`
- `assets/pdfs/hybrid-experiment-design.pdf`
- `assets/pdfs/hybrid-hashtag-convergence.pdf`
- `assets/pdfs/hybrid-narrative-alignment.pdf`
- `assets/pdfs/hybrid-perspective-shifts.pdf`
- `assets/season_and_trim_chat_transcript.txt`

### Legacy page and its assets

The unlinked original `projects/network-narratives.html` and its three unique images were moved to `trash/`. A lightweight redirect remains at the original URL and sends visitors to `projects/pnas-network-consensus.html`, with a normal fallback link. The archived images are:

- `assets/images/Belief Dynamics.png`
- `assets/images/Echochambers.png`
- `assets/images/Social Learning.png`

## Minimal modular structure

Keep the generated HTML URLs and all approved project content. Use a small build step to assemble shared templates and copy only an explicit set of public files:

```text
src/
  pages/          Existing page bodies, preserving project-specific content
  partials/       Document head, navigation, footer, card and figure patterns
  data/           Navigation, project topics/cards, homepage updates
assets/           Public images, CV/résumé, CSS and browser JavaScript
scripts/          Static build and local-link validation
site-dist/        Generated public site only
```

1. Extract the head/navigation/footer first; confirm generated pages preserve their URLs, text, links, figure order and active navigation.
2. Centralize card titles, cover paths, topic memberships and update entries. Keep long research narratives as readable page content rather than forcing them into large JSON strings.
3. Keep one stylesheet organized around tokens, layout and shared components, with the existing small brand-specific overrides. Remove selectors only after checking their use in HTML and JavaScript, including modal and filtered states.
4. Keep interaction features independently initialized: navigation, filters, figure/game viewers, digest signup and sharing. Build-time modules can still emit one small browser script.
5. A concise README, dependency-free build command, and output-directory configuration were added during SEO preparation. Git and Vercel ignore files exclude the local archive and generated output. No Git repository has been initialized in this folder.
6. The static build now publishes only `site-dist/`. Vercel selects this with `outputDirectory`; GitHub Pages can use the same generated artifact. See [Vercel’s output-directory documentation](https://vercel.com/docs/project-configuration/vercel-json#outputdirectory).

A small static build is sufficient. Client-side fetching of shared navigation would make it depend on JavaScript; build-time partials keep the site usable immediately.

## Mobile check and changes — 20 October

All 20 HTML pages have device-width viewport metadata. All 19 regular site pages use the shared stylesheet; the research notebook is a standalone export. The stylesheet provides single-column phone layouts for research cards, New Media cards, consulting cards, forms, timeline entries, and paired figure explanations. Images scale within their containers, and the model comparison table has its own horizontal scrolling wrapper.

Changes made in the 20 October mobile pass:

- Added Share beside Email. Native sharing is tried first, then clipboard copying, then a selectable URL if clipboard access is unavailable. Email and Share stay together when the social row wraps. Cancelling a native share does not copy anything.
- Enlarged social buttons and phone topic controls to at least 44 px high.
- Made each mobile navigation link fill its row; added Escape-to-close, focus return, an explicit navigation control relationship, and a usable no-JavaScript navigation layout.
- Increased consulting inputs to 16 px and constrained their widths to reduce phone auto-zoom and overflow.
- Allowed long words/URLs to wrap instead of widening the page; protected the hero identity column from intrinsic-width overflow.
- Deferred two below-the-fold research images with lazy loading. The portrait and project heading images remain eager.
- Fixed the notebook’s relative favicon path.

Verification: JavaScript syntax check; simulated interaction checks for native sharing, cancellation, clipboard failure, manual copying, repeat clicks and navigation; existing research filter, game viewer and digest signup checks; unique HTML IDs; and local link/fragment validation. Local references and fragments pass. The provider endpoint `/_vercel/insights/script.js` is excluded from filesystem missing-file checks; confirm analytics configuration when deploying.

**Limit:** Chrome and Chromium could not start under this environment’s process restrictions. This is a source and behavior audit, not a rendered mobile-browser or physical-device test. Before release, inspect phone portrait/landscape, expanded navigation, figure zoom and game player on a real device. Native share availability also depends on the browser/device and secure context; see [MDN Web Share](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/share) and [clipboard documentation](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText).

## Original image sizes identified in the audit

| Referenced image | Current size | Next step |
| --- | --- | --- |
| `assets/images/Networks Overview Figure.png` | 8.1 MiB; 14,400 × 8,100 | Smaller display image with original retained for figure zoom |
| `assets/images/hunter-priniski.jpeg` | 1.5 MiB; 3,089 × 2,048 | Smaller portrait display variants, preserving the unfiltered original |
| `assets/images/journal.pone.0263191.g008.PNG` | 1.3 MiB; 2,005 × 1,137 | Compressed display version; preserve scientific legibility |

The table records original image sizes. A later SEO pass added smaller display copies while preserving these originals for existing URLs and figure zoom. The site also loads Google Fonts, an external arXiv figure, and notebook-specific libraries; network availability was not comprehensively checked. No Lighthouse score or rendered mobile certification is claimed.


## SEO preparation completed — 21 October

- Added consistent titles, page-specific descriptions, canonical links, Open Graph/Twitter previews and valid JSON-LD to all 19 content pages, including the research notebook. Visible page text is unchanged.
- Added Person and WebSite entities, with About represented as a ProfilePage. The metadata connects the public name, UCLA affiliation, Google Scholar, GitHub and Substack profiles already used on the site.
- Added a 19-URL sitemap and a robots file referencing it. Filter queries consolidate to the base research portfolio URL, and the retired page is excluded from the sitemap.
- Configured permanent Vercel redirects from `/index.html`, the retired network page, and the `www` hostname. Production domain attachment and DNS remain account-level work.
- Added an original-portrait sharing card at 1200 × 630, a raster export of the existing New Sage logo for previews, and correctly dimensioned project sharing images.
- Added responsive display copies for the portrait, the large network overview figure and the April–May COVID survey figure. The portrait’s rotation metadata was normalized during export; no visual filter was applied. Original image hashes are unchanged.
- Added intrinsic dimensions to local page images to reserve their layout space.
- Centralized metadata in `scripts/site-metadata.json`; `node scripts/update-seo.mjs` updates it reproducibly. A dependency-free static build (`node scripts/build.mjs`) publishes only `site-dist/`, excluding the archive, source scripts, and maintenance documents. Both ignore files exclude the local archive and generated build output.

Image sizes: portrait display 50,831 bytes (original 1,554,887); network overview 485,886 bytes at 1600 px or 169,545 at 800 px (original 8,470,677); COVID figure 1,085,263 bytes at 1600 px or 410,684 at 800 px (original 1,330,173). Responsive selection depends on viewport and display density.

Checks cover metadata uniqueness, JSON-LD parsing, intrinsic/social-image dimensions, srcset widths, sitemap URLs, metadata-generation repeatability, internal references, and preservation of visible text, links, styles, browser scripts and original images. The exported notebook’s MathJax and Mermaid scripts are preserved exactly.

At Hunter’s request, the launch verification pass is deferred: no phone/browser interaction checks, forms, deployed routing checks or deployment were performed. Search Console verification/submission and external profile link updates require the live domain and the relevant account access; the README records those steps.
