import { SESSIONS } from "./data.mjs";

import { formatAmerican, formatPct } from "./odds.mjs";

import { ASSETS, THEME, displayName, playerHeadshotUrl } from "./branding.mjs";



function esc(s) {

  return String(s ?? "")

    .replace(/&/g, "&amp;")

    .replace(/</g, "&lt;")

    .replace(/>/g, "&gt;")

    .replace(/"/g, "&quot;");

}



function edgeClass(edge) {

  if (edge == null || !Number.isFinite(edge)) return "";

  if (edge >= 3) return "edge-pos";

  if (edge <= -3) return "edge-neg";

  return "edge-neutral";

}



function playerCell(p) {

  const url = playerHeadshotUrl(p.name);

  const teamCls = p.team === "USA" ? "usa" : "int";

  const img = url

    ? `<img class="headshot" src="${esc(url)}" alt="" />`

    : `<div class="headshot headshot-fallback ${teamCls}">${esc(displayName(p.name).charAt(0))}</div>`;

  return `<div class="player-cell">${img}<span>${esc(displayName(p.name))}</span></div>`;

}



function teamRosterCards(players, team) {

  const cls = team === "USA" ? "usa" : "int";

  const badge = team === "USA" ? ASSETS.usaBadge : ASSETS.intBadge;

  const label = team === "USA" ? "U.S. Team" : "International Team";

  const sorted = [...players].sort((a, b) => b.mu_sg - a.mu_sg);

  const cards = sorted

    .map((p) => {

      const url = playerHeadshotUrl(p.name);

      const img = url

        ? `<img src="${esc(url)}" alt="${esc(displayName(p.name))}" />`

        : `<div class="roster-fallback">${esc(displayName(p.name).charAt(0))}</div>`;

      return `<div class="roster-card ${cls}">

        ${img}

        <div class="roster-name">${esc(displayName(p.name))}</div>

        <div class="roster-sg">${p.mu_sg.toFixed(2)} μ SG</div>

      </div>`;

    })

    .join("");

  return `<div class="roster-panel ${cls}">

    <div class="roster-head">

      <img class="team-badge" src="${esc(badge)}" alt="" />

      <div><div class="roster-title">${esc(label)}</div><div class="roster-sub">${sorted.length} players</div></div>

    </div>

    <div class="roster-grid">${cards}</div>

  </div>`;

}



export function renderReportHtml({ field, sim, markets, dkMeta, methodology }) {

  const generated = new Date().toISOString().slice(0, 16).replace("T", " ");

  const withBook = markets.filter((m) => m.book_american != null);

  const positiveEdge = withBook.filter((m) => (m.edge_pct ?? 0) >= 2).slice(0, 10);

  const usaRetain = sim.cupProb.usa_win + sim.cupProb.usa_retain_tie * 0.5;



  const abilityRows = sim.abilityTable

    .map((p) => {

      const teamCls = p.team === "USA" ? "tag-usa" : "tag-int";

      return `<tr>

        <td>${playerCell(p)}</td>

        <td><span class="team-tag ${teamCls}">${esc(p.team === "USA" ? "USA" : "INT")}</span></td>

        <td class="num">${p.mu_sg.toFixed(3)}</td>

      </tr>`;

    })

    .join("");



  const sessionRows = SESSIONS.map((s) => {

    const sw = sim.sessionWins[s.id];

    return `<tr><td>${esc(s.label)}</td><td class="num usa-num">${formatPct(sw.usa)}</td><td class="num int-num">${formatPct(sw.int)}</td><td class="num">${formatPct(sw.halve)}</td></tr>`;

  }).join("");



  const scoreBandRows = sim.scoreBands

    .map((b) => `<tr><td>${esc(b.score)}</td><td class="num">${formatPct(b.prob, 2)}</td></tr>`)

    .join("");



  const topScorerRows = sim.topScorer

    .slice(0, 12)

    .map((p) => {

      const teamCls = p.team === "USA" ? "tag-usa" : "tag-int";

      return `<tr>

        <td>${playerCell(p)}</td>

        <td><span class="team-tag ${teamCls}">${esc(p.team === "USA" ? "USA" : "INT")}</span></td>

        <td class="num">${p.exp_points.toFixed(2)}</td>

        <td class="num">${formatPct(p.top_scorer_prob)}</td>

        <td class="num">${formatPct(p.prob_ge3_pts)}</td>

      </tr>`;

    })

    .join("");



  const marketRows = markets

    .slice(0, 55)

    .map((m) => {

      const edge = m.edge_pct;

      return `<tr>

        <td>${esc(m.market)}</td>

        <td>${esc(m.selection)}</td>

        <td class="num">${formatPct(m.model_prob)}</td>

        <td class="num">${formatAmerican(m.model_american)}</td>

        <td class="num">${m.book_american != null ? formatAmerican(m.book_american) : "—"}</td>

        <td class="num ${edgeClass(edge)}">${edge != null ? `${edge.toFixed(1)}%` : "—"}</td>

        <td class="num">${m.ev_per_unit != null ? m.ev_per_unit.toFixed(3) : "—"}</td>

      </tr>`;

    })

    .join("");



  const edgeHighlight = positiveEdge.length

    ? positiveEdge

        .map(

          (m) =>

            `<li><strong>${esc(m.selection)}</strong> <span class="market-tag">${esc(m.market)}</span><br/>

            Model ${formatPct(m.model_prob)} · DK ${formatAmerican(m.book_american)} · <span class="edge-pos">+${m.edge_pct.toFixed(1)}% edge</span></li>`,

        )

        .join("")

    : "<li>No positive edges ≥2% vs DraftKings on matched markets.</li>";



  const usaRoster = teamRosterCards(field.usa, "USA");

  const intRoster = teamRosterCards(field.intl, "INT");



  return `<!DOCTYPE html>

<html lang="en">

<head>

<meta charset="utf-8"/>

<title>Presidents Cup 2026 — Match-Play Model Report</title>

<link rel="preconnect" href="https://www.presidentscup.com"/>

<style>

  @page { size: letter; margin: 0; }

  * { box-sizing: border-box; }

  :root {

    --pc-red: ${THEME.red};

    --pc-gold: ${THEME.gold};

    --pc-gold-dark: ${THEME.goldDark};

    --pc-green: ${THEME.green};

    --pc-black: ${THEME.black};

    --pc-gray: ${THEME.gray};

    --pc-usa: ${THEME.usa};

    --pc-int: ${THEME.int};

  }

  body {

    font-family: "Segoe UI", "Helvetica Neue", Arial, sans-serif;

    color: var(--pc-black);

    font-size: 10pt;

    line-height: 1.45;

    margin: 0;

    background: ${THEME.offWhite};

  }

  .hero {

    position: relative;

    min-height: 220px;

    background: var(--pc-black) url("${ASSETS.hero}") center/cover no-repeat;

    color: #fff;

    padding: 28px 36px 24px;

    overflow: hidden;

  }

  .hero::before {

    content: "";

    position: absolute; inset: 0;

    background: linear-gradient(135deg, rgba(25,25,26,.92) 0%, rgba(217,0,10,.55) 45%, rgba(0,96,57,.45) 100%);

  }

  .hero-inner { position: relative; z-index: 1; display: flex; justify-content: space-between; align-items: flex-start; gap: 20px; }

  .hero-badges { display: flex; gap: 12px; align-items: center; }

  .hero-badges img { width: 56px; height: 56px; object-fit: contain; filter: drop-shadow(0 2px 8px rgba(0,0,0,.4)); }

  .hero h1 {

    font-size: 26pt; font-weight: 800; margin: 0 0 4px;

    letter-spacing: -0.03em; text-transform: uppercase;

    background: linear-gradient(90deg, #fff 0%, var(--pc-gold) 100%);

    -webkit-background-clip: text; -webkit-text-fill-color: transparent;

    background-clip: text;

  }

  .hero .subtitle { font-size: 11pt; opacity: .92; margin: 0 0 8px; }

  .hero .meta { font-size: 8.5pt; opacity: .75; max-width: 520px; }

  .hero-date {

    text-align: right; font-size: 9pt; opacity: .85;

    border-left: 3px solid var(--pc-gold); padding-left: 14px;

  }

  .hero-date strong { display: block; font-size: 11pt; color: var(--pc-gold); }



  .content { padding: 22px 32px 28px; }



  .cup-showdown {

    display: grid; grid-template-columns: 1fr auto 1fr; gap: 16px; align-items: stretch;

    margin: -36px 32px 20px; position: relative; z-index: 2;

  }

  .cup-team {

    background: #fff; border-radius: 10px; padding: 16px 18px;

    box-shadow: 0 4px 20px rgba(0,0,0,.12); text-align: center;

    border-top: 4px solid var(--pc-gray);

  }

  .cup-team.usa { border-top-color: var(--pc-usa); }

  .cup-team.int { border-top-color: var(--pc-int); }

  .cup-team .pct { font-size: 28pt; font-weight: 800; line-height: 1; margin: 6px 0; }

  .cup-team.usa .pct { color: var(--pc-usa); }

  .cup-team.int .pct { color: var(--pc-int); }

  .cup-team .lbl { font-size: 8pt; text-transform: uppercase; letter-spacing: .08em; color: var(--pc-gray); }

  .cup-team img { width: 48px; height: 48px; object-fit: contain; }

  .cup-vs {

    display: flex; align-items: center; justify-content: center;

    font-size: 18pt; font-weight: 900; color: var(--pc-gold-dark);

  }



  .kpi-row { display: grid; grid-template-columns: repeat(5, 1fr); gap: 10px; margin: 0 0 18px; }

  .kpi {

    background: #fff; border-radius: 8px; padding: 12px 14px;

    border: 1px solid ${THEME.grayLight}; border-bottom: 3px solid var(--pc-gold);

  }

  .kpi .label { font-size: 7.5pt; text-transform: uppercase; letter-spacing: .07em; color: var(--pc-gray); }

  .kpi .value { font-size: 16pt; font-weight: 800; color: var(--pc-black); margin-top: 3px; }



  h2 {

    font-size: 12pt; margin: 20px 0 10px; color: var(--pc-black);

    text-transform: uppercase; letter-spacing: .04em;

    border-bottom: 2px solid var(--pc-gold); padding-bottom: 5px;

  }

  h2::before { content: "▸ "; color: var(--pc-red); }



  .method {

    background: #fff; border-left: 4px solid var(--pc-green);

    padding: 12px 16px; margin: 12px 0; font-size: 9pt;

    border-radius: 0 8px 8px 0; box-shadow: 0 1px 4px rgba(0,0,0,.06);

  }



  .edge-box {

    background: linear-gradient(135deg, #fff 0%, #fff8f0 100%);

    border: 1px solid var(--pc-gold); border-radius: 10px;

    padding: 14px 18px; margin: 12px 0;

  }

  .edge-box ul { margin: 8px 0 0; padding-left: 0; list-style: none; }

  .edge-box li { padding: 8px 0; border-bottom: 1px solid ${THEME.grayLight}; font-size: 9.5pt; }

  .edge-box li:last-child { border-bottom: none; }

  .market-tag {

    display: inline-block; font-size: 7.5pt; background: var(--pc-black); color: #fff;

    padding: 2px 6px; border-radius: 3px; margin-left: 4px; vertical-align: middle;

  }



  .rosters { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin: 14px 0; }

  .roster-panel {

    background: #fff; border-radius: 10px; overflow: hidden;

    box-shadow: 0 2px 12px rgba(0,0,0,.08);

  }

  .roster-panel.usa { border-top: 4px solid var(--pc-usa); }

  .roster-panel.int { border-top: 4px solid var(--pc-int); }

  .roster-head {

    display: flex; align-items: center; gap: 12px;

    padding: 12px 14px; background: var(--pc-black); color: #fff;

  }

  .roster-panel.usa .roster-head { background: linear-gradient(90deg, #2a0809, var(--pc-black)); }

  .roster-panel.int .roster-head { background: linear-gradient(90deg, #0a1a2e, var(--pc-black)); }

  .team-badge { width: 40px; height: 40px; object-fit: contain; }

  .roster-title { font-weight: 700; font-size: 11pt; }

  .roster-sub { font-size: 8pt; opacity: .7; }

  .roster-grid {

    display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; padding: 10px;

  }

  .roster-card {

    text-align: center; border-radius: 8px; overflow: hidden;

    background: ${THEME.offWhite}; border: 1px solid ${THEME.grayLight};

  }

  .roster-card img { width: 100%; aspect-ratio: 1; object-fit: cover; display: block; background: #ddd; }

  .roster-fallback {

    width: 100%; aspect-ratio: 1; display: flex; align-items: center; justify-content: center;

    font-size: 22pt; font-weight: 800; color: #fff;

  }

  .roster-card.usa .roster-fallback { background: var(--pc-usa); }

  .roster-card.int .roster-fallback { background: var(--pc-int); }

  .roster-name { font-size: 7.5pt; font-weight: 700; padding: 4px 4px 0; line-height: 1.2; }

  .roster-sg { font-size: 7pt; color: var(--pc-gray); padding: 0 4px 6px; }



  table { width: 100%; border-collapse: collapse; margin: 8px 0 14px; font-size: 9pt; background: #fff; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 6px rgba(0,0,0,.06); }

  th { text-align: left; background: var(--pc-black); color: #fff; padding: 7px 9px; font-weight: 700; font-size: 8pt; text-transform: uppercase; letter-spacing: .04em; }

  th.num, td.num { text-align: right; font-variant-numeric: tabular-nums; }

  td { padding: 6px 9px; border-bottom: 1px solid ${THEME.grayLight}; vertical-align: middle; }

  tr:nth-child(even) td { background: #fafafa; }

  .usa-num { color: var(--pc-usa); font-weight: 600; }

  .int-num { color: var(--pc-int); font-weight: 600; }



  .player-cell { display: flex; align-items: center; gap: 8px; }

  .headshot {

    width: 32px; height: 32px; border-radius: 50%; object-fit: cover;

    border: 2px solid ${THEME.grayLight}; flex-shrink: 0;

  }

  .headshot-fallback {

    display: flex; align-items: center; justify-content: center;

    font-size: 11pt; font-weight: 800; color: #fff;

  }

  .headshot-fallback.usa { background: var(--pc-usa); border-color: var(--pc-usa); }

  .headshot-fallback.int { background: var(--pc-int); border-color: var(--pc-int); }



  .team-tag {

    display: inline-block; font-size: 7pt; font-weight: 800; padding: 2px 7px;

    border-radius: 3px; letter-spacing: .05em;

  }

  .tag-usa { background: rgba(217,0,10,.12); color: var(--pc-usa); }

  .tag-int { background: rgba(0,132,255,.12); color: var(--pc-int); }



  .edge-pos { color: var(--pc-green); font-weight: 700; }

  .edge-neg { color: var(--pc-red); font-weight: 700; }

  .edge-neutral { color: var(--pc-gray); }



  .course-banner {

    margin: 16px 0; border-radius: 10px; overflow: hidden; height: 100px;

    background: url("${ASSETS.course}") center/cover no-repeat;

    position: relative;

  }

  .course-banner::after {

    content: "Medinah Country Club · Chicago, IL · September 22–27, 2026";

    position: absolute; inset: 0; display: flex; align-items: flex-end;

    padding: 10px 14px; font-size: 9pt; font-weight: 700; color: #fff;

    background: linear-gradient(transparent, rgba(0,0,0,.75));

  }



  .footer {

    margin-top: 24px; padding: 14px 32px; background: var(--pc-black); color: rgba(255,255,255,.65);

    font-size: 8pt;

  }

  .footer a { color: var(--pc-gold); }

  .note { color: var(--pc-gray); font-size: 8.5pt; font-style: italic; }



  @media print {

    .page-break { page-break-before: always; }

    .hero { min-height: 180px; }

    .roster-grid { grid-template-columns: repeat(4, 1fr); }

  }

</style>

</head>

<body>

  <div class="hero">

    <div class="hero-inner">

      <div>

        <div class="hero-badges">

          <img src="${esc(ASSETS.usaBadge)}" alt="USA"/>

          <img src="${esc(ASSETS.intBadge)}" alt="International"/>

        </div>

        <h1>Presidents Cup 2026</h1>

        <p class="subtitle">Match-Play Monte Carlo Model · ${esc(field.course)}</p>

        <p class="meta">${sim.nSims.toLocaleString()} simulations · Alpha Caddie · Methodology adapted from <a href="https://datagolf.com/ryder-cup-blog/" style="color:var(--pc-gold)">DataGolf</a></p>

      </div>

      <div class="hero-date">

        <strong>September 22–27, 2026</strong>

        Medinah Country Club<br/>Chicago, Illinois<br/>

        <span style="opacity:.6">Generated ${esc(generated)}</span>

      </div>

    </div>

  </div>



  <div class="cup-showdown">

    <div class="cup-team usa">

      <img src="${esc(ASSETS.usaBadge)}" alt=""/>

      <div class="lbl">U.S. Team wins Cup</div>

      <div class="pct">${formatPct(usaRetain)}</div>

      <div class="lbl">${formatPct(sim.cupProb.usa_win)} outright · ${formatPct(sim.cupProb.usa_retain_tie)} retain</div>

    </div>

    <div class="cup-vs">VS</div>

    <div class="cup-team int">

      <img src="${esc(ASSETS.intBadge)}" alt=""/>

      <div class="lbl">International wins Cup</div>

      <div class="pct">${formatPct(sim.cupProb.int_win)}</div>

      <div class="lbl">μ SG ${sim.teamAbility.int.toFixed(2)} vs USA ${sim.teamAbility.usa.toFixed(2)}</div>

    </div>

  </div>



  <div class="content">

    <div class="kpi-row">

      <div class="kpi"><div class="label">Most likely score</div><div class="value">${esc(sim.scoreBands[0]?.score || "—")}</div></div>

      <div class="kpi"><div class="label">USA Fri fourball</div><div class="value">${formatPct(sim.sessionWins.fri_fourball?.usa)}</div></div>

      <div class="kpi"><div class="label">USA Sun singles</div><div class="value">${formatPct(sim.sessionWins.sun_singles?.usa)}</div></div>

      <div class="kpi"><div class="label">Top scorer</div><div class="value" style="font-size:11pt">${esc(displayName(sim.topScorer[0]?.name || ""))}</div></div>

      <div class="kpi"><div class="label">DK lines matched</div><div class="value">${withBook.length}</div></div>

    </div>



    <div class="course-banner"></div>



    <div class="method">${esc(methodology)}</div>



    <h2>Recommended Edges vs DraftKings</h2>

    <div class="edge-box"><ul>${edgeHighlight}</ul></div>

    ${dkMeta.note ? `<p class="note">${esc(dkMeta.note)}</p>` : ""}



    <h2>2026 Teams</h2>

    <div class="rosters">${usaRoster}${intRoster}</div>



    <div class="page-break"></div>

    <h2>Team Ability Rankings</h2>

    <table>

      <thead><tr><th>Player</th><th>Team</th><th class="num">μ SG / round</th></tr></thead>

      <tbody>${abilityRows}</tbody>

    </table>



    <h2>Session Win Probabilities</h2>

    <table>

      <thead><tr><th>Session</th><th class="num">USA</th><th class="num">International</th><th class="num">Tie</th></tr></thead>

      <tbody>${sessionRows}</tbody>

    </table>



    <h2>Top Cup Point Scorers</h2>

    <table>

      <thead><tr><th>Player</th><th>Team</th><th class="num">E[pts]</th><th class="num">Top scorer</th><th class="num">O 2.5 pts</th></tr></thead>

      <tbody>${topScorerRows}</tbody>

    </table>



    <h2>Most Likely Final Scorelines</h2>

    <table>

      <thead><tr><th>USA–INT points</th><th class="num">Probability</th></tr></thead>

      <tbody>${scoreBandRows}</tbody>

    </table>



    <div class="page-break"></div>

    <h2>Model vs Book — All Priced Markets</h2>

    <table>

      <thead><tr>

        <th>Market</th><th>Selection</th>

        <th class="num">Model %</th><th class="num">Model</th>

        <th class="num">DK</th><th class="num">Edge</th><th class="num">EV/u</th>

      </tr></thead>

      <tbody>${marketRows}</tbody>

    </table>

  </div>



  <div class="footer">

    Alpha Caddie match-play model · Inspired by <a href="https://www.presidentscup.com/">Presidents Cup</a> official branding · Not affiliated with PGA Tour, DataGolf, or DraftKings.<br/>

    Player imagery &amp; team badges sourced from <a href="https://www.presidentscup.com/">presidentscup.com</a> · DK: ${esc(dkMeta.url || "")} (${dkMeta.props?.length || 0} lines)

  </div>

</body>

</html>`;

}


