/**
 * Merge Courtside Round Props into projections.props.
 */
import { fetchCourtsideOuProps } from "./courtside-ou-props.mjs";
import { canonicalizeDkOuPropsAgainstProjections } from "./merge-dk-round-props.mjs";
import { filterPpPropsToProjectionField } from "./pp-field-align.mjs";
import {
  mergeBookSourceIntoProps,
  num,
  preserveBookSourceProps,
  propRowHasPostableLine,
} from "./pickem-ou-shared.mjs";

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

export function preserveCourtsideRoundProps(payload, nonCsProps) {
  return preserveBookSourceProps(payload, nonCsProps, "courtside");
}

export async function refreshCourtsideRoundProps(payload) {
  if (process.env.GOLF_SKIP_CS_OU === "1") {
    return { props: payload?.props || [], nCs: 0, csError: "skipped (GOLF_SKIP_CS_OU=1)" };
  }
  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let csProps = [];
  let csError;
  try {
    const hit = await fetchCourtsideOuProps({ payload, targetRound: modelRound });
    csProps = (hit.props || []).map((r) => ({ ...r, source: "courtside" }));
    canonicalizeDkOuPropsAgainstProjections(csProps, payload.players);
    csError = hit.error;
    if (!csProps.length && csError) console.warn("[cs-round-props]", csError);
  } catch (e) {
    csError = e.message;
    console.warn("[cs-round-props] skipped:", e.message);
  }
  if (!csProps.length) {
    const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "courtside" && propRowHasPostableLine(r),
    );
    if (prior.length && !envTruthy("GOLF_REQUIRE_CS_OU")) {
      csProps = prior;
      console.warn(`[cs-round-props] Courtside fetch returned 0 rows — keeping ${prior.length} prior`);
    }
  }
  csProps = csProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(csProps, payload.players);
  csProps = filterPpPropsToProjectionField(csProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], csProps, "courtside");
  return { props: merged, nCs: csProps.length, csError };
}
