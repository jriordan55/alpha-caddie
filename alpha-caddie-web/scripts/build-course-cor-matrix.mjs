#!/usr/bin/env node
/**
 * Rebuild data/course_cor_matrix.json from player_round_history.json.
 *   node scripts/build-course-cor-matrix.mjs [--force]
 */
import { mainBuildCourseCorMatrix } from "./plus4-cor-score.mjs";

mainBuildCourseCorMatrix().catch((e) => {
  console.error(e);
  process.exit(1);
});
