/**
 * Merge Kalshi round-score props into projections.props.
 */
import { fetchKalshiOuProps } from "./kalshi-ou-props.mjs";
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

export function preserveKalshiRoundProps(payload, nonKlProps) {
  return preserveBookSourceProps(payload, nonKlProps, "kalshi");
}

export async function refreshKalshiRoundProps(payload) {
  if (process.env.GOLF_SKIP_KL_OU === "1") {
    return { props: payload?.props || [], nKl: 0, klError: "skipped (GOLF_SKIP_KL_OU=1)" };
  }
  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let klProps = [];
  let klError;
  try {
    const hit = await fetchKalshiOuProps({ payload, targetRound: modelRound });
    klProps = (hit.props || []).map((r) => ({ ...r, source: "kalshi" }));
    canonicalizeDkOuPropsAgainstProjections(klProps, payload.players);
    klError = hit.error;
    if (!klProps.length && klError) console.warn("[kl-round-props]", klError);
  } catch (e) {
    klError = e.message;
    console.warn("[kl-round-props] skipped:", e.message);
  }
  if (!klProps.length) {
    const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "kalshi" && propRowHasPostableLine(r),
    );
    if (prior.length && !envTruthy("GOLF_REQUIRE_KL_OU")) {
      klProps = prior;
      console.warn(`[kl-round-props] Kalshi fetch returned 0 rows — keeping ${prior.length} prior`);
    }
  }
  klProps = klProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(klProps, payload.players);
  klProps = filterPpPropsToProjectionField(klProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], klProps, "kalshi");
  return { props: merged, nKl: klProps.length, klError };
}
