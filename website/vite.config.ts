import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig, loadEnv } from "vite";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/** Optional: copy datagolf.local.example.json → datagolf.local.json and set apiKey (no .env needed). */
function loadKeyFromLocalJson(): string {
  try {
    const p = path.join(__dirname, "datagolf.local.json");
    if (!fs.existsSync(p)) return "";
    const raw = fs.readFileSync(p, "utf8");
    const j = JSON.parse(raw) as { apiKey?: string };
    return typeof j.apiKey === "string" ? j.apiKey.trim() : "";
  } catch {
    return "";
  }
}

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), "");
  const dgKey = (env.DATAGOLF_API_KEY || "").trim() || loadKeyFromLocalJson();

  return {
    publicDir: "public",
    server: {
      proxy: {
        "/api/datagolf": {
          target: "https://feeds.datagolf.com",
          changeOrigin: true,
          secure: true,
          rewrite: (path) => path.replace(/^\/api\/datagolf\/?/, "/"),
          configure(proxy) {
            proxy.on("proxyReq", (proxyReq) => {
              if (!dgKey) return;
              const p = proxyReq.path || "";
              if (p.includes("key=")) return;
              const sep = p.includes("?") ? "&" : "?";
              proxyReq.path = `${p}${sep}key=${encodeURIComponent(dgKey)}`;
            });
          },
        },
      },
    },
  };
});
