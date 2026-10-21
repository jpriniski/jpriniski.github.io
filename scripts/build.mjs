// Build a static-only publication directory, excluding local source/archive files.
import './update-seo.mjs';
import { cp, mkdir, readdir, rm } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const output = path.join(root, 'site-dist');
await rm(output, { recursive: true, force: true });
await mkdir(output, { recursive: true });

const rootFiles = (await readdir(root)).filter(file => file.endsWith('.html'));
for (const file of [...rootFiles, 'favicon.ico', 'robots.txt', 'sitemap.xml']) {
  await cp(path.join(root, file), path.join(output, file));
}
for (const directory of ['assets', 'css']) {
  await cp(path.join(root, directory), path.join(output, directory), {
    recursive: true,
    filter: source => !path.basename(source).startsWith('.')
  });
}
await mkdir(path.join(output, 'projects'), { recursive: true });
for (const file of await readdir(path.join(root, 'projects'))) {
  if (file.endsWith('.html')) await cp(path.join(root, 'projects', file), path.join(output, 'projects', file));
}
console.log('Built static website in site-dist/; archive and maintenance files excluded.');
