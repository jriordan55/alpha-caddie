/**
 * Betsperts betting visuals — Course breakdown / Course fit / Odds Screen.
 * Loads data/betsperts_betting_viz.json and paints the 11 betting viz cards.
 */
(function () {
  "use strict";

  let VIZ = null;
  let loadPromise = null;
  let state = {
    playerDg: null,
    market: "Total score",
  };

  function num(v, fb) {
    const n = Number(String(v ?? "").replace(/,/g, "").trim());
    return Number.isFinite(n) ? n : fb;
  }

  function normCourseNameKey(raw) {
    if (typeof window.normCourseNameKey === "function") return window.normCourseNameKey(raw);
    let s = String(raw || "")
      .trim()
      .toLowerCase();
    s = s.replace(/\([^)]*\)/g, " ");
    s = s.replace(/\b(g\.?\s*c\.?|c\.?\s*c\.?)\b/gi, " ");
    s = s.replace(/[^a-z0-9]+/g, " ").replace(/\s+/g, " ").trim();
    return s;
  }

  async function ensureViz() {
    if (VIZ) return VIZ;
    if (loadPromise) return loadPromise;
    loadPromise = (async () => {
      try {
        const res = await fetch("data/betsperts_betting_viz.json", { cache: "force-cache" });
        if (!res.ok) throw new Error(String(res.status));
        VIZ = await res.json();
      } catch (e) {
        console.warn("[betting-viz] load failed", e);
        VIZ = { courses: {}, playersByDg: {} };
      }
      return VIZ;
    })();
    return loadPromise;
  }

  function resolveCourseRec(courseLabel) {
    if (!VIZ?.courses) return null;
    const raw = String(courseLabel || "");
    const parts = raw.split(/\s*[·;|]\s*/).map((s) => s.trim()).filter(Boolean);
    const keys = parts.length ? parts : [raw];
    for (const part of keys) {
      const ck = normCourseNameKey(part);
      if (VIZ.courses[ck]) return VIZ.courses[ck];
      for (const [k, rec] of Object.entries(VIZ.courses)) {
        if (k.includes(ck) || ck.includes(k)) return rec;
      }
    }
    return null;
  }

  function activeCourseLabel() {
    const m = window.DATA?.meta || {};
    return String(m.course_used || window.DATA?.course_used || "").trim();
  }

  function fieldPlayers() {
    const rows = Array.isArray(window.DATA?.players) ? window.DATA.players : [];
    return rows.filter((r) => Number.isFinite(num(r.dg_id, NaN)));
  }

  function playerRec(dg) {
    return VIZ?.playersByDg?.[String(Math.round(num(dg, NaN)))] || null;
  }

  function fillEmpty(el, msg) {
    if (!el) return;
    el.innerHTML = `<p class="bbv-empty">${msg || "Betsperts data not loaded for this venue."}</p>`;
  }

  /** 1. Hole scoring map */
  function renderHoleMap(host, rec) {
    if (!host) return;
    const holes = Array.isArray(rec?.hole_stats) ? rec.hole_stats : [];
    if (!holes.length) {
      fillEmpty(host, "No hole stats for this course yet.");
      return;
    }
    const maxAbs = Math.max(0.15, ...holes.map((h) => Math.abs(num(h.avg_plus_minus, 0))));
    host.innerHTML = `<div class="bbv-hole-grid" role="list">${holes
      .map((h) => {
        const pm = num(h.avg_plus_minus, 0);
        const t = Math.max(-1, Math.min(1, pm / maxAbs));
        const bg =
          t < 0
            ? `rgba(46, 160, 100, ${0.15 + Math.abs(t) * 0.55})`
            : t > 0
              ? `rgba(200, 70, 70, ${0.15 + Math.abs(t) * 0.55})`
              : "rgba(128,128,128,0.12)";
        const tip = `H${h.hole} Par ${h.par} · ${h.yards}y · AVG ${num(h.avg, NaN).toFixed(2)} (${pm >= 0 ? "+" : ""}${pm.toFixed(2)}) · Birdie ${num(h.birdies_pct, 0).toFixed(1)}% · Bogey ${num(h.bogeys_pct, 0).toFixed(1)}%`;
        return `<button type="button" class="bbv-hole" role="listitem" style="background:${bg}" title="${tip}" data-hole="${h.hole}">
          <span class="bbv-hole-n">${h.hole}</span>
          <span class="bbv-hole-pm">${pm >= 0 ? "+" : ""}${pm.toFixed(2)}</span>
          <span class="bbv-hole-meta">Par ${h.par}</span>
        </button>`;
      })
      .join("")}</div>
      <p class="bbv-caption">Green = under par (birdie holes) · Red = over par. Hover for eagle/birdie/bogey rates — use for birdie &amp; bogey O/U leans.</p>`;
  }

  /** 2. Course vs Tour gauges */
  function renderGauges(host, rec) {
    if (!host) return;
    const ms = rec?.metric_summary || {};
    const gauges = [
      {
        label: "Driving Acc",
        course: ms.driving_accuracy_pct,
        tour: ms.tour_driving_accuracy_pct,
        unit: "%",
        market: "Fairways",
      },
      { label: "GIR", course: ms.gir_pct, tour: ms.tour_gir_pct, unit: "%", market: "GIR" },
      { label: "BoB", course: ms.bob_pct, tour: ms.tour_bob_pct, unit: "%", market: "Birdies" },
      {
        label: "Scrambling",
        course: ms.scrambling_pct,
        tour: null,
        unit: "%",
        market: "Bogeys",
      },
    ].filter((g) => Number.isFinite(g.course));
    if (!gauges.length) {
      fillEmpty(host, "No course metrics vs Tour yet.");
      return;
    }
    host.innerHTML = `<div class="bbv-gauge-row">${gauges
      .map((g) => {
        const d = Number.isFinite(g.tour) ? g.course - g.tour : 0;
        const lean = !Number.isFinite(g.tour)
          ? "—"
          : Math.abs(d) < 1
            ? "Neutral"
            : d > 0
              ? "Over lean"
              : "Under lean";
        const cls = lean.startsWith("Over") ? "is-over" : lean.startsWith("Under") ? "is-under" : "";
        return `<div class="bbv-gauge ${cls}">
          <div class="bbv-gauge-label">${g.label} <span>· ${g.market}</span></div>
          <div class="bbv-gauge-vals"><strong>${g.course.toFixed(1)}${g.unit}</strong>
            ${Number.isFinite(g.tour) ? `<em>Tour ${g.tour.toFixed(1)}${g.unit}</em>` : ""}</div>
          <div class="bbv-gauge-delta">${Number.isFinite(g.tour) ? `${d >= 0 ? "+" : ""}${d.toFixed(1)} vs Tour · ${lean}` : lean}</div>
        </div>`;
      })
      .join("")}</div>`;
  }

  /** 3. Correlated course ladder */
  function renderCorrelated(host, rec) {
    if (!host) return;
    const list = Array.isArray(rec?.correlated) ? rec.correlated : [];
    if (!list.length) {
      fillEmpty(host, "No correlated courses listed.");
      return;
    }
    host.innerHTML = `<ol class="bbv-corr-list">${list
      .slice(0, 10)
      .map((name, i) => {
        const other = resolveCourseRec(name);
        const rtp = num(other?.score_rtp, NaN);
        const bob = num(other?.metric_summary?.bob_pct, NaN);
        const fw = num(other?.metric_summary?.driving_accuracy_pct, NaN);
        return `<li>
          <span class="bbv-corr-rank">${i + 1}</span>
          <span class="bbv-corr-name">${name}</span>
          <span class="bbv-corr-stats">${Number.isFinite(rtp) ? `RTP ${rtp >= 0 ? "+" : ""}${rtp.toFixed(2)}` : "—"}
            ${Number.isFinite(bob) ? ` · BoB ${bob.toFixed(1)}%` : ""}
            ${Number.isFinite(fw) ? ` · Acc ${fw.toFixed(1)}%` : ""}</span>
        </li>`;
      })
      .join("")}</ol>
      <p class="bbv-caption">Betsperts correlated courses — compare historical scoring / BoB / accuracy when venue sample is thin.</p>`;
  }

  /** 4. Condition fit radar (canvas) */
  function renderConditionRadar(canvas, dg) {
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    const w = canvas.width;
    const h = canvas.height;
    ctx.clearRect(0, 0, w, h);
    const pre = playerRec(dg);
    const course = resolveCourseRec(activeCourseLabel());
    const axes = [];
    const venueGreen = course?.grass?.greens?.[0];
    if (pre?.greenGrass?.length) {
      for (const g of pre.greenGrass.slice(0, 4)) {
        axes.push({ label: g.surface, value: g.sg, highlight: venueGreen && g.surface.toLowerCase().includes(String(venueGreen).toLowerCase()) });
      }
    }
    if (pre?.courseLength?.length) {
      for (const g of pre.courseLength.slice(0, 3)) {
        axes.push({ label: g.label, value: g.sg, highlight: false });
      }
    }
    if (pre?.scoringCondition?.length) {
      for (const g of pre.scoringCondition.slice(0, 3)) {
        axes.push({ label: g.label, value: g.sg, highlight: false });
      }
    }
    if (axes.length < 3) {
      ctx.fillStyle = getComputedStyle(document.body).getPropertyValue("--muted") || "#888";
      ctx.font = "13px Inter, sans-serif";
      ctx.fillText("Need Betsperts player splits for this golfer.", 24, h / 2);
      return;
    }
    const cx = w / 2;
    const cy = h / 2 + 8;
    const R = Math.min(w, h) * 0.36;
    const n = axes.length;
    const vals = axes.map((a) => a.value);
    const maxAbs = Math.max(0.4, ...vals.map((v) => Math.abs(v)));
    const theme = getComputedStyle(document.documentElement);
    const grid = theme.getPropertyValue("--border") || "rgba(128,128,128,0.35)";
    const ink = theme.getPropertyValue("--text") || "#e8e8e8";
    const accent = theme.getPropertyValue("--accent") || "#b8956a";
    ctx.strokeStyle = grid;
    ctx.fillStyle = ink;
    for (let ring = 1; ring <= 3; ring++) {
      ctx.beginPath();
      for (let i = 0; i < n; i++) {
        const ang = -Math.PI / 2 + (i * 2 * Math.PI) / n;
        const rr = (R * ring) / 3;
        const x = cx + Math.cos(ang) * rr;
        const y = cy + Math.sin(ang) * rr;
        if (i === 0) ctx.moveTo(x, y);
        else ctx.lineTo(x, y);
      }
      ctx.closePath();
      ctx.stroke();
    }
    ctx.beginPath();
    for (let i = 0; i < n; i++) {
      const ang = -Math.PI / 2 + (i * 2 * Math.PI) / n;
      const t = (axes[i].value / maxAbs + 1) / 2;
      const rr = R * Math.max(0.08, Math.min(1, t));
      const x = cx + Math.cos(ang) * rr;
      const y = cy + Math.sin(ang) * rr;
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.closePath();
    ctx.fillStyle = "rgba(184,149,106,0.28)";
    ctx.fill();
    ctx.strokeStyle = accent;
    ctx.stroke();
    ctx.font = "11px Inter, sans-serif";
    ctx.textAlign = "center";
    for (let i = 0; i < n; i++) {
      const ang = -Math.PI / 2 + (i * 2 * Math.PI) / n;
      const x = cx + Math.cos(ang) * (R + 18);
      const y = cy + Math.sin(ang) * (R + 18);
      ctx.fillStyle = axes[i].highlight ? accent : ink;
      ctx.fillText(axes[i].label.slice(0, 12), x, y);
      ctx.fillStyle = ink;
      ctx.fillText(axes[i].value >= 0 ? `+${axes[i].value.toFixed(2)}` : axes[i].value.toFixed(2), x, y + 12);
    }
  }

  /** 5. Filter heat table */
  function renderFilterHeat(host, courseRec) {
    if (!host) return;
    const players = fieldPlayers().slice(0, 40);
    const venueGreen = courseRec?.grass?.greens?.[0] || "";
    const cols = [
      { key: "green", label: venueGreen || "Greens", get: (pr) => {
        const hit = (pr?.greenGrass || []).find((g) => !venueGreen || g.surface.toLowerCase().includes(venueGreen.toLowerCase()));
        return hit ? hit.sg : NaN;
      } },
      { key: "len", label: "Length fit", get: (pr) => {
        const rows = pr?.courseLength || [];
        if (!rows.length) return NaN;
        return rows.reduce((s, r) => s + r.sg, 0) / rows.length;
      } },
      { key: "cond", label: "Scoring fit", get: (pr) => {
        const rows = pr?.scoringCondition || [];
        if (!rows.length) return NaN;
        return rows.reduce((s, r) => s + r.sg, 0) / rows.length;
      } },
    ];
    const body = players
      .map((p) => {
        const dg = Math.round(num(p.dg_id, NaN));
        const pr = playerRec(dg);
        const cells = cols.map((c) => {
          const v = c.get(pr);
          const cls = !Number.isFinite(v) ? "" : v > 0.15 ? "is-hot" : v < -0.15 ? "is-cold" : "";
          return `<td class="num ${cls}">${Number.isFinite(v) ? (v >= 0 ? "+" : "") + v.toFixed(2) : "—"}</td>`;
        });
        return `<tr data-dg="${dg}"><td>${p.player_name || p.name || dg}</td>${cells.join("")}</tr>`;
      })
      .join("");
    host.innerHTML = `<div class="table-wrap bbv-heat-wrap"><table class="data-table bbv-heat">
      <thead><tr><th>Golfer</th>${cols.map((c) => `<th class="num">${c.label}</th>`).join("")}</tr></thead>
      <tbody>${body || `<tr><td colspan="4">No field players</td></tr>`}</tbody>
    </table></div>
    <p class="bbv-caption">SG under venue-relevant Rabbit Hole splits (green surface / length / scoring). Hot = positive fit for matchups &amp; outs.</p>`;
  }

  /** 6. Green surface sparkline */
  function renderGreenSpark(host, dg, courseRec) {
    if (!host) return;
    const pr = playerRec(dg);
    const rows = pr?.greenGrass || [];
    const venue = courseRec?.grass?.greens?.[0] || "";
    if (!rows.length) {
      fillEmpty(host, "No greenGrass splits for this golfer.");
      return;
    }
    const maxAbs = Math.max(0.2, ...rows.map((r) => Math.abs(r.sg)));
    host.innerHTML = `<div class="bbv-spark-row">${rows
      .map((r) => {
        const h = Math.round((Math.abs(r.sg) / maxAbs) * 56) + 8;
        const on = venue && r.surface.toLowerCase().includes(venue.toLowerCase());
        return `<div class="bbv-spark ${on ? "is-venue" : ""}" title="${r.surface}: ${r.sg.toFixed(3)} SG (${r.n} rds)">
          <div class="bbv-spark-bar ${r.sg >= 0 ? "pos" : "neg"}" style="height:${h}px"></div>
          <span class="bbv-spark-lbl">${r.surface}</span>
          <span class="bbv-spark-val">${r.sg >= 0 ? "+" : ""}${r.sg.toFixed(2)}</span>
        </div>`;
      })
      .join("")}</div>
      <p class="bbv-caption">Venue greens: <strong>${venue || "—"}</strong>. Highlighted bar = this week’s surface (putting / score edge).</p>`;
  }

  /** 7. Model vs book waterfall */
  function renderWaterfall(host, dg) {
    if (!host) return;
    const rows = fieldPlayers();
    const p = rows.find((r) => Math.round(num(r.dg_id, NaN)) === Math.round(num(dg, NaN))) || rows[0];
    if (!p) {
      fillEmpty(host, "No projection row.");
      return;
    }
    const basis = window.DATA?.meta?.projection_course_basis || window.DATA?.projection_course_basis || {};
    const par = num(window.DATA?.meta?.course_par_18 ?? window.DATA?.course_par_18, 72);
    const mu = num(p.mu_sg ?? p.implied_mu_sg, 0);
    const modelScore = par - mu;
    const book = num(p.total_score_book_line ?? p.score_book_line ?? p.line, NaN);
    const cor = num(p.plus4_cor?.cor_delta, 0);
    const extra = num(p.plus4_cor?.extra_stroke, 0);
    const venueStp = num(basis.venue_avg_score_to_par, 0);
    const steps = [
      { label: "Par", v: par },
      { label: "− skill μ", v: -mu },
      { label: "COR lift", v: -cor },
      { label: "Surface/extra", v: extra },
      { label: "Venue STP context", v: venueStp * 0 },
    ];
    let run = 0;
    const bars = steps.map((s) => {
      run += s.v;
      return { ...s, run };
    });
    host.innerHTML = `<div class="bbv-waterfall">
      ${bars
        .map(
          (s) => `<div class="bbv-wf-step"><span>${s.label}</span><strong>${s.v >= 0 ? "+" : ""}${s.v.toFixed(2)}</strong><em>→ ${s.run.toFixed(2)}</em></div>`,
        )
        .join("")}
      <div class="bbv-wf-final">Model μ score <strong>${modelScore.toFixed(2)}</strong>
        ${Number.isFinite(book) ? ` · Book <strong>${book.toFixed(1)}</strong> · Edge <strong>${(book - modelScore).toFixed(2)}</strong> (book−model)` : " · No book line"}
      </div>
    </div>
    <p class="bbv-caption">Decomposition for ${p.player_name || "player"} total score. Positive book−model → model likes Under.</p>`;
  }

  /** 8. Market lean chips */
  function renderLeanChips(host, rec) {
    if (!host) return;
    const chips = Array.isArray(rec?.leans) ? rec.leans : [];
    if (!chips.length) {
      fillEmpty(host, "No course metric leans yet.");
      return;
    }
    host.innerHTML = `<div class="bbv-chip-row">${chips
      .map((c) => {
        const cls = c.lean === "Over" ? "is-over" : c.lean === "Under" ? "is-under" : "is-neutral";
        return `<div class="bbv-chip ${cls}"><span class="bbv-chip-mkt">${c.market}</span><span class="bbv-chip-lean">${c.lean}</span><span class="bbv-chip-detail">${c.detail}</span></div>`;
      })
      .join("")}</div>`;
  }

  /** 9. Mismatch scatter */
  function renderMismatch(canvas) {
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    const w = canvas.width;
    const h = canvas.height;
    ctx.clearRect(0, 0, w, h);
    const rows = fieldPlayers()
      .map((p) => {
        const dg = Math.round(num(p.dg_id, NaN));
        const par = num(window.DATA?.meta?.course_par_18 ?? window.DATA?.course_par_18, 72);
        const model = par - num(p.mu_sg, 0);
        const book = num(p.total_score_book_line ?? p.score_book_line ?? p.line, NaN);
        const pr = playerRec(dg);
        const course = resolveCourseRec(activeCourseLabel());
        const venueGreen = course?.grass?.greens?.[0] || "";
        let fit = 0;
        if (pr?.greenGrass?.length && venueGreen) {
          const hit = pr.greenGrass.find((g) => g.surface.toLowerCase().includes(venueGreen.toLowerCase()));
          if (hit) fit = hit.sg;
        }
        return { name: p.player_name, model, book, fit, dg };
      })
      .filter((r) => Number.isFinite(r.book) && Number.isFinite(r.model));
    if (rows.length < 3) {
      ctx.fillStyle = "#888";
      ctx.font = "13px Inter,sans-serif";
      ctx.fillText("Need book total-score lines on the field.", 20, h / 2);
      return;
    }
    const pad = 36;
    const xs = rows.map((r) => r.book);
    const ys = rows.map((r) => r.model);
    const minX = Math.min(...xs) - 0.5;
    const maxX = Math.max(...xs) + 0.5;
    const minY = Math.min(...ys) - 0.5;
    const maxY = Math.max(...ys) + 0.5;
    const xScale = (x) => pad + ((x - minX) / (maxX - minX || 1)) * (w - pad * 2);
    const yScale = (y) => h - pad - ((y - minY) / (maxY - minY || 1)) * (h - pad * 2);
    ctx.strokeStyle = "rgba(128,128,128,0.35)";
    ctx.beginPath();
    ctx.moveTo(pad, h - pad);
    ctx.lineTo(w - pad, h - pad);
    ctx.moveTo(pad, pad);
    ctx.lineTo(pad, h - pad);
    ctx.stroke();
    // y=x
    ctx.setLineDash([4, 4]);
    ctx.beginPath();
    ctx.moveTo(xScale(minX), yScale(minX));
    ctx.lineTo(xScale(maxX), yScale(maxX));
    ctx.stroke();
    ctx.setLineDash([]);
    for (const r of rows) {
      const x = xScale(r.book);
      const y = yScale(r.model);
      const t = Math.max(-1, Math.min(1, r.fit / 0.5));
      ctx.fillStyle =
        t > 0 ? `rgba(46,160,100,${0.4 + Math.abs(t) * 0.5})` : t < 0 ? `rgba(200,70,70,${0.4 + Math.abs(t) * 0.5})` : "rgba(150,150,150,0.6)";
      ctx.beginPath();
      ctx.arc(x, y, 5, 0, Math.PI * 2);
      ctx.fill();
    }
    ctx.fillStyle = getComputedStyle(document.documentElement).getPropertyValue("--text") || "#ccc";
    ctx.font = "11px Inter,sans-serif";
    ctx.fillText("Book line →", w / 2 - 20, h - 10);
    ctx.save();
    ctx.translate(12, h / 2);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText("Model μ", 0, 0);
    ctx.restore();
  }

  /** 10 + 11 window + season */
  function renderWindows(host, dg) {
    if (!host) return;
    const pr = playerRec(dg);
    const avg = pr?.windows?.average || {};
    const tot = pr?.windows?.total || {};
    const roundKeys = ["round-12", "round-24", "round-50", "round-100", "all"];
    const seasonKeys = Object.keys(avg)
      .filter((k) => k.startsWith("season-"))
      .sort();
    const fmt = (o, k) => {
      const row = o[k];
      if (!row || !Number.isFinite(row.sg_tot)) return "—";
      return `${row.sg_tot >= 0 ? "+" : ""}${row.sg_tot.toFixed(2)}`;
    };
    host.innerHTML = `
      <div class="bbv-windows">
        <div class="bbv-win-block">
          <h4>Average SG:TOT by window</h4>
          <div class="bbv-win-row">${roundKeys
            .map((k) => `<div class="bbv-win-cell"><span>${k.replace("round-", "L").replace("all", "Career")}</span><strong>${fmt(avg, k)}</strong></div>`)
            .join("")}</div>
        </div>
        <div class="bbv-win-block">
          <h4>Total SG:TOT (same windows)</h4>
          <div class="bbv-win-row">${["round-50", "round-100", "all"]
            .map((k) => `<div class="bbv-win-cell"><span>${k.replace("round-", "L").replace("all", "Career")}</span><strong>${fmt(tot, k)}</strong></div>`)
            .join("")}</div>
        </div>
        <div class="bbv-win-block">
          <h4>Season stack (Average)</h4>
          <div class="bbv-season-row">${(seasonKeys.length ? seasonKeys : ["—"])
            .map((k) => {
              if (k === "—") return `<div class="bbv-season">—</div>`;
              const v = avg[k]?.sg_tot;
              const h = Number.isFinite(v) ? Math.min(64, Math.max(8, Math.abs(v) * 22)) : 8;
              return `<div class="bbv-season" title="${k}: ${fmt(avg, k)}">
                <div class="bbv-season-bar ${v >= 0 ? "pos" : "neg"}" style="height:${h}px"></div>
                <span>${k.replace("season-", "")}</span>
              </div>`;
            })
            .join("")}</div>
        </div>
      </div>
      <p class="bbv-caption">Rate (Average) vs volume (Total) — hot L12 with flat career = steam risk on overs.</p>`;
  }

  function selectedDg() {
    const sel =
      document.getElementById("bbv-player") ||
      document.getElementById("course-fit-player") ||
      document.getElementById("ev-filter-golfer");
    const v = sel ? num(sel.value, NaN) : NaN;
    if (Number.isFinite(v)) return Math.round(v);
    const first = fieldPlayers()[0];
    return first ? Math.round(num(first.dg_id, NaN)) : NaN;
  }

  function populatePlayerSelect(sel) {
    if (!sel) return;
    const cur = sel.value;
    const players = fieldPlayers()
      .slice()
      .sort((a, b) => String(a.player_name || "").localeCompare(String(b.player_name || "")));
    sel.innerHTML = players
      .map((p) => {
        const dg = Math.round(num(p.dg_id, NaN));
        return `<option value="${dg}">${p.player_name || dg}</option>`;
      })
      .join("");
    if (cur && [...sel.options].some((o) => o.value === cur)) sel.value = cur;
  }

  async function renderAll() {
    await ensureViz();
    const courseLabel = activeCourseLabel();
    const rec = resolveCourseRec(courseLabel);
    const dg = selectedDg();
    state.playerDg = dg;

    populatePlayerSelect(document.getElementById("bbv-player"));
    populatePlayerSelect(document.getElementById("bbv-player-fit"));

    const title = document.getElementById("bbv-course-label");
    if (title) title.textContent = rec?.name || courseLabel || "—";

    renderHoleMap(document.getElementById("bbv-hole-map"), rec);
    renderGauges(document.getElementById("bbv-gauges"), rec);
    renderCorrelated(document.getElementById("bbv-correlated"), rec);
    renderLeanChips(document.getElementById("bbv-lean-chips"), rec);
    renderLeanChips(document.getElementById("bbv-lean-chips-ev"), rec);
    renderConditionRadar(document.getElementById("bbv-condition-radar"), dg);
    renderFilterHeat(document.getElementById("bbv-filter-heat"), rec);
    renderGreenSpark(document.getElementById("bbv-green-spark"), dg, rec);
    renderWaterfall(document.getElementById("bbv-waterfall"), dg);
    renderMismatch(document.getElementById("bbv-mismatch"));
    renderWindows(document.getElementById("bbv-windows"), dg);
  }

  function bind() {
    document.getElementById("bbv-player")?.addEventListener("change", () => {
      const dg = num(document.getElementById("bbv-player").value, NaN);
      if (Number.isFinite(dg)) {
        const fit = document.getElementById("bbv-player-fit");
        if (fit) fit.value = String(Math.round(dg));
      }
      void renderAll();
    });
    document.getElementById("bbv-player-fit")?.addEventListener("change", () => {
      const dg = num(document.getElementById("bbv-player-fit").value, NaN);
      if (Number.isFinite(dg)) {
        const a = document.getElementById("bbv-player");
        if (a) a.value = String(Math.round(dg));
      }
      void renderAll();
    });
  }

  window.AlphaCaddieBettingViz = {
    ensure: ensureViz,
    render: renderAll,
    bind,
    resolveCourse: resolveCourseRec,
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bind);
  } else {
    bind();
  }
})();
