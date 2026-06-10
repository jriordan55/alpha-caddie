/**
 * fetch:dg writes projections.json with export metadata at the top level (event_name,
 * display_round, course_used, …). Some post-processors wrote a nested `meta` blob.
 * Read/write export fields through this helper so the web app (applyPayload) sees them.
 */

export function usesFlatProjectionExportSchema(proj) {
  if (!proj || typeof proj !== "object") return false;
  return (
    proj.event_name != null ||
    proj.course_used != null ||
    proj.display_round != null ||
    proj.datagolf_field_date_start != null
  );
}

/** Object where export-level metadata should be read/written. */
export function projectionExportMeta(proj) {
  if (!proj || typeof proj !== "object") return proj;
  if (usesFlatProjectionExportSchema(proj)) {
    hoistNestedProjectionMeta(proj);
    return proj;
  }
  if (!proj.meta || typeof proj.meta !== "object") {
    proj.meta = {};
  }
  return proj.meta;
}

/** Copy nested proj.meta onto the root when fetch:dg flat schema is in use. */
export function hoistNestedProjectionMeta(proj) {
  if (!proj || typeof proj !== "object") return proj;
  const nested = proj.meta;
  if (!nested || typeof nested !== "object") return proj;
  for (const [k, v] of Object.entries(nested)) {
    if (proj[k] === undefined && v !== undefined) proj[k] = v;
  }
  return proj;
}

/** Remove nested `meta` after hoisting (call before writing projections.json). */
export function flattenProjectionExportMeta(proj) {
  if (!proj || typeof proj !== "object") return proj;
  hoistNestedProjectionMeta(proj);
  if (usesFlatProjectionExportSchema(proj) && proj.meta && typeof proj.meta === "object") {
    delete proj.meta;
  }
  return proj;
}
