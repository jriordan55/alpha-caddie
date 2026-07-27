/**
 * Merge Underdog round projections into projections.props.
 */
import { fetchUnderdogOuProps } from "./underdog-ou-props.mjs";
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

export function preserveUnderdogRoundProps(payload, nonUdProps) {
  return preserveBookSourceProps(payload, nonUdProps, "underdog");
}

/**
 * @returns {{ props: object[], nUd: number, udError?: string }}
 */
export async function refreshUnderdogRoundProps(payload) {
  if (process.env.GOLF_SKIP_UD_OU === "1") {
    return { props: payload?.props || [], nUd: 0, udError: "skipped (GOLF_SKIP_UD_OU=1)" };
  }

  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let udProps = [];
  let udError;
  try {
    const hit = await fetchUnderdogOuProps({ payload, targetRound: modelRound });
    udProps = (hit.props || []).map((r) => ({ ...r, source: "underdog" }));
    canonicalizeDkOuPropsAgainstProjections(udProps, payload.players);
    udError = hit.error;
    if (!udProps.length && udError) console.warn("[ud-round-props]", udError);
  } catch (e) {
    udError = e.message;
    console.warn("[ud-round-props] skipped:", e.message);
  }

  if (!udProps.length) {
    const priorUd = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "underdog" && propRowHasPostableLine(r),
    );
    if (priorUd.length && !envTruthy("GOLF_REQUIRE_UD_OU")) {
      udProps = priorUd;
      console.warn(
        `[ud-round-props] Underdog fetch returned 0 rows — keeping ${priorUd.length} prior underdog props`,
      );
    }
  }

  udProps = udProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(udProps, payload.players);
  udProps = filterPpPropsToProjectionField(udProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], udProps, "underdog");
  return {
    props: merged,
    nUd: udProps.length,
    udError,
  };
}
