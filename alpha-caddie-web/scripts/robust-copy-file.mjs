/**
 * Windows/OneDrive-safe file copy for publish mirrors.
 * errno -4094 UNKNOWN and EBUSY/EPERM/EACCES are common when the dest is locked.
 *
 * Strategy per attempt:
 *   1) copyFileSync
 *   2) unlink dest (best-effort) + copyFileSync
 *   3) read + write to dest.tmp + rename (replace)
 */
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  unlinkSync,
  writeFileSync,
} from "fs";
import { dirname } from "path";
import { setTimeout as delay } from "timers/promises";

function isTransientFsError(err) {
  const code = String(err?.code || "");
  const errno = err?.errno;
  if (code === "EBUSY" || code === "EPERM" || code === "EACCES" || code === "EAGAIN") return true;
  if (code === "UNKNOWN" || errno === -4094) return true;
  return false;
}

function tryUnlink(path) {
  try {
    if (existsSync(path)) unlinkSync(path);
    return true;
  } catch {
    return false;
  }
}

function copyViaBuffer(src, dest) {
  const dir = dirname(dest);
  mkdirSync(dir, { recursive: true });
  const tmp = `${dest}.tmp-${process.pid}-${Date.now()}`;
  writeFileSync(tmp, readFileSync(src));
  try {
    renameSync(tmp, dest);
  } catch {
    // Windows often refuses rename over a locked target — write in place after unlink.
    tryUnlink(dest);
    try {
      renameSync(tmp, dest);
    } catch {
      writeFileSync(dest, readFileSync(tmp));
      tryUnlink(tmp);
    }
  }
}

/**
 * @param {string} src
 * @param {string} dest
 * @param {{ attempts?: number, baseDelayMs?: number, label?: string }} [opts]
 * @returns {Promise<{ ok: boolean, attempts: number, error?: Error }>}
 */
export async function robustCopyFile(src, dest, opts = {}) {
  const attempts = Math.max(1, Number(opts.attempts) || 12);
  const baseDelayMs = Math.max(50, Number(opts.baseDelayMs) || 350);
  const label = opts.label || dest;
  if (!existsSync(src)) {
    return { ok: false, attempts: 0, error: new Error(`missing source: ${src}`) };
  }
  mkdirSync(dirname(dest), { recursive: true });

  let lastErr = null;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    try {
      copyFileSync(src, dest);
      return { ok: true, attempts: attempt };
    } catch (e1) {
      lastErr = e1;
    }

    try {
      tryUnlink(dest);
      copyFileSync(src, dest);
      return { ok: true, attempts: attempt };
    } catch (e2) {
      lastErr = e2;
    }

    try {
      copyViaBuffer(src, dest);
      return { ok: true, attempts: attempt };
    } catch (e3) {
      lastErr = e3;
    }

    if (attempt < attempts) {
      const wait = baseDelayMs * attempt;
      if (attempt === 1 || attempt % 3 === 0) {
        console.warn(
          `[robust-copy] ${label}: attempt ${attempt}/${attempts} failed (${lastErr?.code || lastErr?.message || lastErr}); retry in ${wait}ms`,
        );
      }
      await delay(wait);
    }
  }

  return { ok: false, attempts, error: lastErr || new Error(`copy failed: ${src} → ${dest}`) };
}

function sleepMsSync(ms) {
  const n = Math.max(0, Math.round(Number(ms) || 0));
  if (n <= 0) return;
  try {
    const sab = new SharedArrayBuffer(4);
    Atomics.wait(new Int32Array(sab), 0, 0, n);
  } catch {
    const end = Date.now() + n;
    while (Date.now() < end) {
      /* spin */
    }
  }
}

/**
 * Sync Windows/OneDrive-safe text write (tmp + rename, with retries).
 * @param {string} dest
 * @param {string|Buffer} contents
 * @param {{ attempts?: number, baseDelayMs?: number, label?: string, encoding?: BufferEncoding }} [opts]
 */
export function robustWriteFileSync(dest, contents, opts = {}) {
  const attempts = Math.max(1, Number(opts.attempts) || 12);
  const baseDelayMs = Math.max(50, Number(opts.baseDelayMs) || 350);
  const label = opts.label || dest;
  const encoding = opts.encoding || "utf8";
  mkdirSync(dirname(dest), { recursive: true });
  let lastErr = null;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    const tmp = `${dest}.tmp-${process.pid}-${Date.now()}-${attempt}`;
    try {
      writeFileSync(tmp, contents, encoding);
      try {
        renameSync(tmp, dest);
        return { ok: true, attempts: attempt };
      } catch (eRename) {
        lastErr = eRename;
        tryUnlink(dest);
        try {
          renameSync(tmp, dest);
          return { ok: true, attempts: attempt };
        } catch (eRename2) {
          lastErr = eRename2;
          writeFileSync(dest, contents, encoding);
          tryUnlink(tmp);
          return { ok: true, attempts: attempt };
        }
      }
    } catch (e) {
      lastErr = e;
      tryUnlink(tmp);
      if (attempt < attempts) {
        const wait = baseDelayMs * attempt;
        if (attempt === 1 || attempt % 3 === 0) {
          console.warn(
            `[robust-write] ${label}: attempt ${attempt}/${attempts} failed (${lastErr?.code || lastErr?.message || lastErr}); retry in ${wait}ms`,
          );
        }
        sleepMsSync(wait);
      }
    }
  }
  return { ok: false, attempts, error: lastErr || new Error(`write failed: ${dest}`) };
}

/**
 * Windows/OneDrive-safe text write (tmp + rename, with retries).
 * @param {string} dest
 * @param {string|Buffer} contents
 * @param {{ attempts?: number, baseDelayMs?: number, label?: string, encoding?: BufferEncoding }} [opts]
 */
export async function robustWriteFile(dest, contents, opts = {}) {
  const attempts = Math.max(1, Number(opts.attempts) || 12);
  const baseDelayMs = Math.max(50, Number(opts.baseDelayMs) || 350);
  const label = opts.label || dest;
  const encoding = opts.encoding || "utf8";
  mkdirSync(dirname(dest), { recursive: true });
  let lastErr = null;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    const tmp = `${dest}.tmp-${process.pid}-${Date.now()}-${attempt}`;
    try {
      writeFileSync(tmp, contents, encoding);
      try {
        renameSync(tmp, dest);
        return { ok: true, attempts: attempt };
      } catch (eRename) {
        lastErr = eRename;
        tryUnlink(dest);
        try {
          renameSync(tmp, dest);
          return { ok: true, attempts: attempt };
        } catch (eRename2) {
          lastErr = eRename2;
          writeFileSync(dest, contents, encoding);
          tryUnlink(tmp);
          return { ok: true, attempts: attempt };
        }
      }
    } catch (e) {
      lastErr = e;
      tryUnlink(tmp);
      if (!isTransientFsError(e) && attempt === 1) {
        /* still retry a few times for UNKNOWN/-4094 */
      }
      if (attempt < attempts) {
        const wait = baseDelayMs * attempt;
        if (attempt === 1 || attempt % 3 === 0) {
          console.warn(
            `[robust-write] ${label}: attempt ${attempt}/${attempts} failed (${lastErr?.code || lastErr?.message || lastErr}); retry in ${wait}ms`,
          );
        }
        await delay(wait);
      }
    }
  }
  return { ok: false, attempts, error: lastErr || new Error(`write failed: ${dest}`) };
}

/**
 * Mirror helpers that must not abort push:live when OneDrive locks website/public.
 * @param {Array<[string, string]>} pairs
 * @param {{ log?: (msg: string) => void, soft?: boolean, attempts?: number }} [opts]
 */
export async function robustMirrorPairs(pairs, opts = {}) {
  const log = opts.log || console.log;
  const soft = opts.soft !== false;
  const attempts = opts.attempts || 12;
  let failed = 0;
  for (const [src, dest] of pairs) {
    if (!existsSync(src)) {
      log(`skip (missing): ${src}`);
      continue;
    }
    const r = await robustCopyFile(src, dest, { attempts, label: dest });
    if (r.ok) {
      log(`mirrored (${r.attempts} attempt${r.attempts === 1 ? "" : "s"}): ${dest}`);
      continue;
    }
    failed += 1;
    const detail = r.error?.message || r.error;
    if (soft) {
      console.warn(`[robust-copy] WARN: could not mirror ${src} → ${dest} (${detail}) — continuing publish`);
      continue;
    }
    throw r.error;
  }
  return { failed };
}
