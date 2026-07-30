/**
 * Merge FanDuel round props into projections.props.
 */
import { fetchFanduelOuProps } from "./fanduel-ou-props.mjs";
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

export function preserveFanduelRoundProps(payload, nonFdProps) {
  return preserveBookSourceProps(payload, nonFdProps, "fanduel");
}

export async function refreshFanduelRoundProps(payload) {
  if (process.env.GOLF_SKIP_FD_OU === "1") {
    return { props: payload?.props || [], nFd: 0, fdError: "skipped (GOLF_SKIP_FD_OU=1)" };
  }
  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let fdProps = [];
  let fdError;
  try {
    const hit = await fetchFanduelOuProps({ payload, targetRound: modelRound });
    fdProps = (hit.props || []).map((r) => ({ ...r, source: "fanduel" }));
    canonicalizeDkOuPropsAgainstProjections(fdProps, payload.players);
    fdError = hit.error;
    if (!fdProps.length && fdError) console.warn("[fd-round-props]", fdError);
  } catch (e) {
    fdError = e.message;
    console.warn("[fd-round-props] skipped:", e.message);
  }
  if (!fdProps.length) {
    const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "fanduel" && propRowHasPostableLine(r),
    );
    if (prior.length && !envTruthy("GOLF_REQUIRE_FD_OU")) {
      fdProps = prior;
      console.warn(`[fd-round-props] FanDuel fetch returned 0 rows — keeping ${prior.length} prior`);
    }
  }
  fdProps = fdProps.filter(propRowHasPostableLine);
  canonicalizeDkOuPropsAgainstProjections(fdProps, payload.players);
  fdProps = filterPpPropsToProjectionField(fdProps, payload.players, modelRound);
  const merged = mergeBookSourceIntoProps(payload?.props || [], fdProps, "fanduel");
  return { props: merged, nFd: fdProps.length, fdError };
}
