# Hunter Priniski’s website

A static portfolio using HTML, one shared stylesheet, and one shared browser script. The dependency-free Node build publishes only website files; no application framework or dependency installation is required.

## Editing and metadata

Edit visible content directly in the HTML files. Search descriptions, browser titles, sharing images, and public profile data live in `scripts/site-metadata.json`.

After updating that file, regenerate the HTML metadata and discovery files:

```sh
node scripts/update-seo.mjs
```

This also adds missing intrinsic image dimensions and connects the three optimized image sets. It preserves page text, figure links, and layout classes. Generated HTML remains usable on both GitHub Pages and Vercel.

`assets/images/optimized/` contains smaller display copies and sharing images. Original figure files remain available to the figure viewer. The portrait exports normalize the source image’s rotation metadata without adding a visual filter.

## Publishing configuration

The preferred public address is `https://priniski.com/`. Update `baseUrl` in the metadata file if this changes. Also update the domain redirect in `vercel.json`.

`vercel.json` preserves the existing `.html` project URLs and configures permanent redirects from `/index.html`, the retired network-narratives page, and the `www` hostname. Attach `priniski.com` and `www.priniski.com` to the Vercel project when publishing; DNS and account configuration happen separately from these files.

Build locally with:

```sh
node scripts/build.mjs
```

Vercel is configured to run that command and publish only `site-dist/`. The build refreshes metadata and copies HTML, assets, CSS, favicon, robots file and sitemap. `trash/`, metadata source files, build scripts and maintenance documentation are outside this public output. The same output can be published through GitHub Pages.

`trash/` contains the reversible cleanup archive. Both it and the generated `site-dist/` directory are excluded by `.gitignore` and `.vercelignore`. Vercel builds its own fresh output from the source files.

## Search discovery after publication

- The sitemap is `https://priniski.com/sitemap.xml`; `robots.txt` references it.
- Verify the production domain in your Google Search Console account and submit that sitemap once the website is live. Domain verification may require a DNS record from your account; it is not created or submitted by the metadata script.
- Use the same production URL on university and scholarly profiles you control. Those external account changes are separate from this repository.

The requested launch verification pass—real-phone layouts, forms, sharing, game player, and deployed routing—is left for Hunter. No deployment was made as part of the SEO update.

References: [Google sitemap guidance](https://developers.google.com/search/docs/crawling-indexing/sitemaps/build-sitemap), [canonical URLs](https://developers.google.com/search/docs/crawling-indexing/consolidate-duplicate-urls), [profile structured data](https://developers.google.com/search/docs/appearance/structured-data/profile-page), and [Vercel redirects](https://vercel.com/docs/routing/redirects).
