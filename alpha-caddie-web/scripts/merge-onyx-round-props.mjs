/**
 * Merge Onyx Odds Round Props into projections.props.
 */
import { fetchOnyxOuProps } from "./onyx-ou-props.mjs";
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

export function preserveOnyxRoundProps(payload, nonOxProps) {
  return preserveBookSourceProps(payload, nonOxProps, "onyx");
}

export async function refreshOnyxRoundProps(payload) {
  if (process.env.GOLF_SKIP_OX_OU === "1") {
    return { props: payload?.props || [], nOx: 0, oxError: "skipped (GOLF_SKIP_OX_OU=1)" };
  }
  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let oxProps = [];
  let oxError;
  try {
    const hit = await fetchOnyxOuProps({ payload, targetRound: modelRound });
    oxProps = (hit.props || []).map((r) => ({ ...r, source: "onyx" }));
    canonicalizeDkOuPropsAgainstProjections(oxProps, payload.players);
    oxError = hit.error;
    if (!oxProps.length && oxError) console.warn("[ox-round-props]", oxError);
  } catch (e) {
    oxError = e.message;
    console.warn("[ox-round-props] skipped:", e.message);
  }
  if (!oxProps.length) {
    const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "onyx" && propRowHasPostableLine(r),
    );
    if (prior.length && !envTruthy("GOLF_REQUIRE_OX_OU")) {
      oxProps = prior;
      console.warn(`[ox-round-props] Onyx fetch returned 0 rows — keeping ${prior.length} prior`);
    }
  }
  oxProps = oxProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(oxProps, payload.players);
  oxProps = filterPpPropsToProjectionField(oxProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], oxProps, "onyx");
  return { props: merged, nOx: oxProps.length, oxError };
}
