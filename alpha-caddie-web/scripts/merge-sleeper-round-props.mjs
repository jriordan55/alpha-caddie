/**
 * Merge Sleeper round projections into projections.props.
 */
import { fetchSleeperOuProps } from "./sleeper-ou-props.mjs";
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

export function preserveSleeperRoundProps(payload, nonSlProps) {
  return preserveBookSourceProps(payload, nonSlProps, "sleeper");
}

/**
 * @returns {{ props: object[], nSl: number, slError?: string }}
 */
export async function refreshSleeperRoundProps(payload) {
  if (process.env.GOLF_SKIP_SL_OU === "1") {
    return { props: payload?.props || [], nSl: 0, slError: "skipped (GOLF_SKIP_SL_OU=1)" };
  }

  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let slProps = [];
  let slError;
  try {
    const hit = await fetchSleeperOuProps({ payload, targetRound: modelRound });
    slProps = (hit.props || []).map((r) => ({ ...r, source: "sleeper" }));
    canonicalizeDkOuPropsAgainstProjections(slProps, payload.players);
    slError = hit.error;
    if (!slProps.length && slError) console.warn("[sl-round-props]", slError);
  } catch (e) {
    slError = e.message;
    console.warn("[sl-round-props] skipped:", e.message);
  }

  if (!slProps.length) {
    const priorSl = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "sleeper" && propRowHasPostableLine(r),
    );
    if (priorSl.length && !envTruthy("GOLF_REQUIRE_SL_OU")) {
      slProps = priorSl;
      console.warn(
        `[sl-round-props] Sleeper fetch returned 0 rows — keeping ${priorSl.length} prior sleeper props`,
      );
    }
  }

  slProps = slProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(slProps, payload.players);
  slProps = filterPpPropsToProjectionField(slProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], slProps, "sleeper");
  return {
    props: merged,
    nSl: slProps.length,
    slError,
  };
}
