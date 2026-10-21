// Regenerate search metadata and discovery files; leave visible page copy intact.
import { readFile, writeFile, access } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const metadata = JSON.parse(await readFile(path.join(root, 'scripts/site-metadata.json'), 'utf8'));
const base = metadata.baseUrl;
const absolute = file => new URL(file, base).href;
const escape = value => String(value).replaceAll('&', '&amp;').replaceAll('"', '&quot;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const json = value => JSON.stringify(value, null, 2).replaceAll('<', '\\u003c');
async function save(file, contents) {
  const target = path.join(root, file);
  let old;
  try { old = await readFile(target, 'utf8'); } catch (error) { if (error.code !== 'ENOENT') throw error; }
  if (contents !== old) await writeFile(target, contents);
}
function fail(message) { throw new Error(message); }

// Read intrinsic dimensions without re-encoding or changing image contents.
async function imageSize(file) {
  const data = await readFile(path.join(root, file));
  if (data.subarray(0, 8).equals(Buffer.from([137,80,78,71,13,10,26,10]))) {
    return [data.readUInt32BE(16), data.readUInt32BE(20)];
  }
  if (data[0] === 0xff && data[1] === 0xd8) {
    const frameMarkers = new Set([0xc0,0xc1,0xc2,0xc3,0xc5,0xc6,0xc7,0xc9,0xca,0xcb,0xcd,0xce,0xcf]);
    let offset = 2;
    while (offset < data.length - 8) {
      if (data[offset++] !== 0xff) continue;
      while (data[offset] === 0xff) offset++;
      const marker = data[offset++];
      if (marker === 1 || (marker >= 0xd0 && marker <= 0xd9)) continue;
      const length = data.readUInt16BE(offset);
      if (frameMarkers.has(marker)) return [data.readUInt16BE(offset + 5), data.readUInt16BE(offset + 3)];
      if (length < 2) break;
      offset += length;
    }
  }
  if (file.toLowerCase().endsWith('.svg')) {
    const svg = data.toString('utf8').match(/<svg\b[^>]*>/)?.[0] || '';
    const width = svg.match(/\bwidth="([\d.]+)/)?.[1];
    const height = svg.match(/\bheight="([\d.]+)/)?.[1];
    if (width && height) return [Number(width), Number(height)];
    const viewBox = svg.match(/\bviewBox="([^"]+)"/)?.[1].trim().split(/[\s,]+/).map(Number);
    if (viewBox?.length === 4) return viewBox.slice(2);
  }
  return fail('Cannot read image dimensions: ' + file);
}
const displayImages = {
  'assets/images/hunter-priniski.jpeg': {
    src: 'assets/images/optimized/hunter-priniski-352.jpg',
    variants: [['assets/images/optimized/hunter-priniski-352.jpg',352], ['assets/images/optimized/hunter-priniski-704.jpg',704]],
    sizes: '176px'
  },
  'assets/images/Networks Overview Figure.png': {
    src: 'assets/images/optimized/networks-overview-1600.png',
    variants: [['assets/images/optimized/networks-overview-800.png',800], ['assets/images/optimized/networks-overview-1600.png',1600]],
    sizes: '(max-width: 808px) calc(100vw - 48px), 760px'
  },
  'assets/images/journal.pone.0263191.g008.PNG': {
    src: 'assets/images/optimized/covid-wave-two-1600.png',
    variants: [['assets/images/optimized/covid-wave-two-800.png',800], ['assets/images/optimized/covid-wave-two-1600.png',1600]],
    sizes: '(max-width: 700px) calc(100vw - 48px), 364px'
  }
};
for (const variant of Object.values(displayImages)) {
  for (const [file] of variant.variants) await access(path.join(root, file));
}

const person = {
  '@type': 'Person', '@id': absolute('#person'), ...metadata.person,
  url: base, image: absolute('assets/images/optimized/hunter-priniski-704.jpg'),
  knowsAbout: ['Narrative agency', 'Human–AI group alignment', 'Media psychology', 'Computational social science', 'New media']
};
const website = {
  '@type': 'WebSite', '@id': absolute('#website'), url: base,
  name: metadata.siteName, inLanguage: 'en', publisher: { '@id': person['@id'] }
};
const seen = new Set();
for (const page of metadata.pages) {
  if (seen.has(page.file)) fail('Duplicate page: ' + page.file);
  seen.add(page.file);
  const canonical = absolute(page.file === 'index.html' ? '' : page.file);
  let html = await readFile(path.join(root, page.file), 'utf8');
  const headMatch = html.match(/<head>([\s\S]*?)<\/head>/);
  if (!headMatch) fail('Missing head in ' + page.file);
  let head = headMatch[1]
    .replace(/\s*<!-- BEGIN GENERATED SEO -->[\s\S]*?<!-- END GENERATED SEO -->\s*/g, '\n')
    .replace(/\s*<title>[\s\S]*?<\/title>\s*/g, '\n')
    .replace(/\s*<meta\b(?=[^>]*(?:name="(?:description|twitter:[^"]+)"|property="og:[^"]+"))[^>]*>\s*/g, '\n')
    .replace(/\s*<link\b(?=[^>]*rel="canonical")[^>]*>\s*/g, '\n')
    .replace(/\s*<!-- Open Graph \/ iMessage link preview -->\s*/g, '\n');
  const image = absolute(page.image);
  const localImage = !/^https?:/.test(page.image);
  const size = localImage ? await imageSize(page.image) : null;
  const mime = page.image.toLowerCase().match(/\.jpe?g$/) ? 'image/jpeg' : 'image/png';
  const pageData = {
    '@type': page.type || 'WebPage', '@id': canonical + '#webpage', url: canonical,
    name: page.title, description: page.description, inLanguage: 'en',
    isPartOf: { '@id': website['@id'] }, author: { '@id': person['@id'] },
    primaryImageOfPage: { '@type': 'ImageObject', url: image, ...(size ? { width: size[0], height: size[1] } : {}) },
    ...(page.type === 'ProfilePage' ? { mainEntity: { '@id': person['@id'] } } : {})
  };
  const graph = page.file === 'index.html' || page.file === 'about.html' ? [person, website, pageData] : [pageData];
  const lines = [
    '  <!-- BEGIN GENERATED SEO -->',
    `  <title>${escape(page.title)}</title>`,
    `  <meta name="description" content="${escape(page.description)}"/>`,
    `  <link rel="canonical" href="${escape(canonical)}"/>`,
    '  <meta property="og:type" content="website"/>',
    `  <meta property="og:site_name" content="${escape(metadata.siteName)}"/>`,
    '  <meta property="og:locale" content="en_US"/>',
    `  <meta property="og:url" content="${escape(canonical)}"/>`,
    `  <meta property="og:title" content="${escape(page.title)}"/>`,
    `  <meta property="og:description" content="${escape(page.description)}"/>`,
    `  <meta property="og:image" content="${escape(image)}"/>`,
    `  <meta property="og:image:type" content="${mime}"/>`,
    `  <meta property="og:image:alt" content="${escape(page.imageAlt)}"/>`,
    ...(size ? [`  <meta property="og:image:width" content="${size[0]}"/>`, `  <meta property="og:image:height" content="${size[1]}"/>`] : []),
    '  <meta name="twitter:card" content="summary_large_image"/>',
    `  <meta name="twitter:title" content="${escape(page.title)}"/>`,
    `  <meta name="twitter:description" content="${escape(page.description)}"/>`,
    `  <meta name="twitter:image" content="${escape(image)}"/>`,
    `  <meta name="twitter:image:alt" content="${escape(page.imageAlt)}"/>`,
    '  <script type="application/ld+json">',
    json({ '@context': 'https://schema.org', '@graph': graph }),
    '  </script>', '  <!-- END GENERATED SEO -->'
  ];
  head = head.trimEnd() + '\n' + lines.join('\n') + '\n';
  html = html.replace(headMatch[0], () => '<head>' + head + '</head>');

  const images = [...html.matchAll(/<img\b[^>]*>/g)];
  for (const match of images) {
    let tag = match[0];
    const src = tag.match(/\bsrc="([^"]+)"/)?.[1];
    if (!src || /^(?:https?:|data:)/.test(src)) continue;
    const local = path.posix.normalize(path.posix.join(path.posix.dirname(page.file), decodeURIComponent(src)));
    const relative = file => path.posix.relative(path.posix.dirname(page.file), file);
    const variant = displayImages[local];
    if (variant) {
      tag = tag.replace(/\bsrc="[^"]*"/, `src="${relative(variant.src)}"`);
      if (!/\bsrcset=/.test(tag)) tag = tag.replace(/\s*\/?>$/, ` srcset="${variant.variants.map(([file,width]) => relative(file) + ' ' + width + 'w').join(', ')}" sizes="${variant.sizes}"/>`);
    }
    if (!/\bwidth=/.test(tag) || !/\bheight=/.test(tag)) {
      const [width, height] = await imageSize(variant?.src || local);
      if (!/\bwidth=/.test(tag)) tag = tag.replace(/\s*\/?>$/, ` width="${width}"/>`);
      if (!/\bheight=/.test(tag)) tag = tag.replace(/\s*\/?>$/, ` height="${height}"/>`);
    }
    html = html.replace(match[0], tag);
  }
  await save(page.file, html);
}
const urls = metadata.pages.map(page => absolute(page.file === 'index.html' ? '' : page.file));
await save('sitemap.xml', '<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n' + urls.map(url => `  <url><loc>${escape(url)}</loc></url>`).join('\n') + '\n</urlset>\n');
await save('robots.txt', `User-agent: *\nAllow: /\n\nSitemap: ${absolute('sitemap.xml')}\n`);
console.log(`Updated metadata for ${seen.size} pages, image dimensions, sitemap.xml and robots.txt.`);
