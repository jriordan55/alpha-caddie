/**
 * Merge PrizePicks round projections into projections.props (alongside DraftKings rows).
 */
import { fetchPrizePicksOuProps } from "./prizepicks-ou-props.mjs";
import { canonicalizeDkOuPropsAgainstProjections } from "./merge-dk-round-props.mjs";
import { sanitizePpRoundProps } from "./pp-ou-line-sanity.mjs";

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function withPropSource(rows, source) {
  return (Array.isArray(rows) ? rows : []).map((r) => ({ ...r, source }));
}

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function propRowHasPostableLine(r) {
  return Number.isFinite(num(r?.line, NaN)) && Number.isFinite(num(r?.over_odds, NaN)) && Number.isFinite(num(r?.under_odds, NaN));
}

/**
 * Keep existing PrizePicks rows when a DK-only props refresh runs (scheduled DK workflow).
 * @param {object} payload projections.json payload before merge
 * @param {object[]} nonPpProps merged DK / CSV / model_fallback rows
 */
export function preservePrizePicksRoundProps(payload, nonPpProps) {
  const priorPp = (Array.isArray(payload?.props) ? payload.props : []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() === "prizepicks" && propRowHasPostableLine(r),
  );
  if (!priorPp.length) return nonPpProps;
  const ppKept = sanitizePpRoundProps(priorPp, nonPpProps);
  if (ppKept.length) {
    console.log(`[pp-round-props] preserved ${ppKept.length} prizepicks row(s) alongside DK refresh`);
  }
  return [...nonPpProps, ...ppKept];
}

/**
 * Fetch PrizePicks props and merge into existing props array (keeps DK / CSV / model_fallback rows).
 * @returns {{ props: object[], nPp: number, ppError?: string }}
 */
export async function refreshPrizePicksRoundProps(payload) {
  const skip = process.env.GOLF_SKIP_PP_OU === "1";
  if (skip) return { props: payload?.props || [], nPp: 0, ppError: "skipped (GOLF_SKIP_PP_OU=1)" };

  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
  let ppProps = [];
  let ppError;
  try {
    const hit = await fetchPrizePicksOuProps({
      payload,
      targetRound: modelRound,
    });
    ppProps = withPropSource(hit.props || [], "prizepicks");
    canonicalizeDkOuPropsAgainstProjections(ppProps, payload.players);
    ppError = hit.error;
    if (!ppProps.length && ppError) console.warn("[pp-round-props]", ppError);
  } catch (e) {
    ppError = e.message;
    console.warn("[pp-round-props] skipped:", e.message);
  }

  if (!ppProps.length) {
    const priorPp = (Array.isArray(payload.props) ? payload.props : []).filter(
      (r) => String(r?.source || "").trim().toLowerCase() === "prizepicks" && propRowHasPostableLine(r),
    );
    const requireFresh = envTruthy("GOLF_REQUIRE_PP_OU");
    if (priorPp.length && !requireFresh) {
      ppProps = priorPp;
      console.warn(
        `[pp-round-props] PrizePicks fetch returned 0 rows — keeping ${priorPp.length} prior prizepicks props`,
      );
    }
  }

  const prior = (Array.isArray(payload.props) ? payload.props : []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() !== "prizepicks",
  );
  ppProps = sanitizePpRoundProps(ppProps.filter(propRowHasPostableLine), prior);
  const merged = [...prior, ...ppProps];
  return {
    props: merged,
    nPp: ppProps.filter(propRowHasPostableLine).length,
    ppError,
  };
}
