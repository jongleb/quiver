import assert from "node:assert/strict";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import test from "node:test";

const builtPage = new URL("../dist/index.html", import.meta.url);

test("the built page tells the complete Quiver product story", () => {
  assert.ok(existsSync(builtPage), "dist/index.html must exist");

  const html = readFileSync(builtPage, "utf8");
  const styles = readdirSync(new URL("../dist/_astro/", import.meta.url))
    .filter((fileName) => fileName.endsWith(".css"))
    .map((fileName) => readFileSync(new URL(`../dist/_astro/${fileName}`, import.meta.url), "utf8"))
    .join("\n");

  for (const sectionId of [
    "pipeline",
    "performance",
    "tidb",
    "masking",
    "correctness",
    "install",
  ]) {
    assert.match(html, new RegExp(`id="${sectionId}"`));
  }

  assert.match(html, /One snapshot\.\s*Every path\./);
  assert.match(html, /Apple M5/);
  assert.match(styles, /prefers-reduced-motion/);
});

test("the page exposes navigation and an accessible install action", () => {
  assert.ok(existsSync(builtPage), "dist/index.html must exist");

  const html = readFileSync(builtPage, "utf8");

  assert.match(html, /<nav[^>]+aria-label="Primary"/);
  assert.match(html, /<main[^>]+id="main-content"/);
  assert.match(html, /data-copy-command/);
  assert.match(html, /aria-live="polite"/);
  assert.match(html, /execCommand\([`'"]copy[`'"]\)/);
  assert.match(html, /href="\/quiver\/_astro\/[^"]+\.css"/);
  assert.match(html, /href="https:\/\/jongleb\.github\.io\/quiver\/cli\.html"/);
});

test("the release workflow overlays the landing page while preserving docs and package repositories", () => {
  const workflow = readFileSync(
    new URL("../../.github/workflows/release.yml", import.meta.url),
    "utf8",
  );

  assert.match(workflow, /working-directory: web/);
  assert.match(workflow, /npm ci/);
  assert.match(workflow, /npm test/);
  assert.match(workflow, /quiver docs site/);
  assert.match(workflow, /cp -a web\/dist\/\. site\//);
  assert.match(workflow, /needs: repository/);
});
