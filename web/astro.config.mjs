import { defineConfig } from "astro/config";

export default defineConfig({
  site: "https://jongleb.github.io",
  base: "/quiver",
  output: "static",
  server: {
    host: true,
    port: 4321,
  },
});
