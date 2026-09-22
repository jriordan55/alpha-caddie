import { join } from "path";
import {
  LEGACY_LIVE_IN_PLAY_FILE,
  LEGACY_PROJECTIONS_FILE,
  liveInPlayFilename,
  projectionsFilename,
  tourFromEnv,
} from "./golf-tours.mjs";

/**
 * Resolve tour-specific projection artifact paths for the current process env.
 * Override with GOLF_PROJECTIONS_FILE / GOLF_LIVE_IN_PLAY_FILE.
 */
export function resolveProjectionPaths(webRoot, env = process.env) {
  const tour = tourFromEnv(env);
  const projectionsFile =
    String(env.GOLF_PROJECTIONS_FILE || "").trim() || projectionsFilename(tour);
  const liveInPlayFile =
    String(env.GOLF_LIVE_IN_PLAY_FILE || "").trim() || liveInPlayFilename(tour);
  return {
    tour,
    projectionsFile,
    liveInPlayFile,
    projectionsPath: join(webRoot, projectionsFile),
    liveInPlayPath: join(webRoot, liveInPlayFile),
    legacyProjectionsPath: join(webRoot, LEGACY_PROJECTIONS_FILE),
    legacyLiveInPlayPath: join(webRoot, LEGACY_LIVE_IN_PLAY_FILE),
  };
}
