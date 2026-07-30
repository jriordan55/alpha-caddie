/**
 * Merge Caesars Round Props into projections.props.
 */
import { fetchCaesarsOuProps } from "./caesars-ou-props.mjs";
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

export function preserveCaesarsRoundProps(payload, nonCzProps) {
  return preserveBookSourceProps(payload, nonCzProps, "caesars");
}

export async function refreshCaesarsRoundProps(payload) {
  if (process.env.GOLF_SKIP_CZR_OU === "1") {
    return { props: payload?.props || [], nCzr: 0, czrError: "skipped (GOLF_SKIP_CZR_OU=1)" };
  }
  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let czProps = [];
  let czrError;
  try {
    const hit = await fetchCaesarsOuProps({ payload, targetRound: modelRound });
    czProps = (hit.props || []).map((r) => ({ ...r, source: "caesars" }));
    canonicalizeDkOuPropsAgainstProjections(czProps, payload.players);
    czrError = hit.error;
    if (!czProps.length && czrError) console.warn("[czr-round-props]", czrError);
  } catch (e) {
    czrError = e.message;
    console.warn("[czr-round-props] skipped:", e.message);
  }
  if (!czProps.length) {
    const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "caesars" && propRowHasPostableLine(r),
    );
    if (prior.length && !envTruthy("GOLF_REQUIRE_CZR_OU")) {
      czProps = prior;
      console.warn(`[czr-round-props] Caesars fetch returned 0 rows — keeping ${prior.length} prior`);
    }
  }
  czProps = czProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(czProps, payload.players);
  czProps = filterPpPropsToProjectionField(czProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], czProps, "caesars");
  return { props: merged, nCzr: czProps.length, czrError };
}
