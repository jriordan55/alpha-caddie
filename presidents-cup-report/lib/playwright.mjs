import { createRequire } from "module";
import { join } from "path";
import { WEB_ROOT } from "./data.mjs";

const require = createRequire(join(WEB_ROOT, "package.json"));
export const { chromium } = require("playwright");
