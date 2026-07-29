/**
 * Browser-safe alignment for legacy round_projection_vs_actual.csv rows
 * missing PrizePicks columns (prevents shifted model/book/result columns).
 */
const PP_DETAIL_INSERT_COUNT = 16;

function parseCsvRowSimple(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

/** @param {string} text */
export function alignDetailCsvText(text) {
  const lines = String(text || "").split(/\r?\n/).filter(Boolean);
  if (lines.length < 2) return text;
  const targetHeaderLine = `${lines[0]}\n`;
  const oldHeader = lines[0].split(",");
  const newHeader = targetHeaderLine.replace(/\n$/, "").split(",");
  const oldIdx = Object.fromEntries(oldHeader.map((h, i) => [h, i]));
  const bookSrcIdx = newHeader.indexOf("book_odds_source");
  const out = [targetHeaderLine.replace(/\n$/, "")];
  for (let i = 1; i < lines.length; i++) {
    let cells = parseCsvRowSimple(lines[i]);
    if (
      bookSrcIdx >= 0 &&
      cells.length + PP_DETAIL_INSERT_COUNT === newHeader.length &&
      cells.length < newHeader.length
    ) {
      cells = [
        ...cells.slice(0, bookSrcIdx + 1),
        ...Array(PP_DETAIL_INSERT_COUNT).fill(""),
        ...cells.slice(bookSrcIdx + 1),
      ];
    }
    const row = new Array(newHeader.length).fill("");
    for (let j = 0; j < newHeader.length; j++) {
      const oi = oldIdx[newHeader[j]];
      if (oi >= 0 && oi < cells.length) row[j] = cells[oi];
      else if (j < cells.length && oldHeader.length !== newHeader.length) row[j] = cells[j];
    }
    out.push(row.map(csvCell).join(","));
  }
  return `${out.join("\n")}\n`;
}
