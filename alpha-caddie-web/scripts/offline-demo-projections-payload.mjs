/**
 * Minimal projections payload when projections.json is missing on disk.
 * Mirrors buildDefaultProjectionsPayload() in app.js (players + props only).
 * Browser applyPayload() fills outrights / matchups from players when empty.
 */

export function buildOfflineDemoProjectionsPayload() {
  const names = [
    "Scheffler, Scottie",
    "McIlroy, Rory",
    "Morikawa, Collin",
    "Schauffele, Xander",
    "Homa, Max",
    "Hatton, Tyrrell",
    "Finau, Tony",
    "Clark, Wyndham",
    "Thomas, Justin",
    "Spieth, Jordan",
    "Rahm, Jon",
    "Fleetwood, Tommy",
  ];
  const countries = ["USA", "NIR", "USA", "USA", "USA", "ENG", "USA", "USA", "USA", "USA", "ESP", "ENG"];
  const players = [];
  names.forEach((player_name, i) => {
    const dg_id = i + 1;
    const country = countries[i] || "USA";
    for (let r = 1; r <= 4; r++) {
      const bump = i * 0.28 + (r - 1) * 0.12;
      const total_score = Math.round((69.4 + bump) * 10) / 10;
      const stp = Math.round((total_score - 72) * 10) / 10;
      players.push({
        dg_id,
        player_name,
        country,
        round: r,
        total_score,
        round_sd: Math.round((2.62 + (i % 7) * 0.04) * 100) / 100,
        score_to_par: stp,
        birdies: Math.round((Math.max(2.5, 4.3 - i * 0.12 - r * 0.05)) * 10) / 10,
        pars: Math.round((Math.min(13, 10.5 + i * 0.08)) * 10) / 10,
        bogeys: Math.round((Math.min(4.5, 2.4 + i * 0.14 + r * 0.04)) * 10) / 10,
        gir: Math.round((Math.max(9, 13 - i * 0.35)) * 10) / 10,
        fairways: Math.round((Math.max(7, 10 - i * 0.15)) * 10) / 10,
        putts: Math.round((28.5 + (i % 5) * 0.15 + r * 0.02) * 10) / 10,
        eagles: Math.round((i < 3 ? 0.25 : 0.12) * 100) / 100,
        doubles: Math.round((0.35 + (i % 4) * 0.08) * 100) / 100,
        win: Math.round(Math.max(0.005, 0.14 - i * 0.009) * 1000) / 1000,
        top_5: Math.round(Math.max(0.02, 0.42 - i * 0.025) * 1000) / 1000,
        top_10: Math.round(Math.max(0.05, 0.58 - i * 0.022) * 1000) / 1000,
        top_20: Math.round(Math.max(0.1, 0.75 - i * 0.018) * 1000) / 1000,
        make_cut: Math.round(Math.max(0.15, 0.92 - i * 0.02) * 1000) / 1000,
        position: i + 1,
        mu_sg: Math.round((72 - total_score) * 0.2 * 10) / 10,
        implied_mu_sg: Math.round((72 - total_score) * 0.2 * 10) / 10,
      });
    }
  });
  const props = [
    { dg_id: 1, player_name: "Scheffler, Scottie", line: 69.5, over_odds: -108, under_odds: -112, market: "Total Score" },
    { dg_id: 2, player_name: "McIlroy, Rory", line: 70.5, over_odds: -110, under_odds: -110, market: "Total Score" },
    { dg_id: 3, player_name: "Morikawa, Collin", line: 4.5, over_odds: -115, under_odds: -105, market: "Birdies" },
    { dg_id: 5, player_name: "Homa, Max", line: 10.5, over_odds: -110, under_odds: -118, market: "Pars" },
    { dg_id: 4, player_name: "Schauffele, Xander", line: 2.5, over_odds: -120, under_odds: -102, market: "Bogeys" },
  ];
  return {
    event_name:
      "Offline stub — set DATAGOLF_API_KEY on the server or run npm run fetch:dg to replace this file.",
    course_used: "Demo venue",
    display_round_label: "",
    updated_at: "",
    source: "offline-demo-stub",
    outrights_model_blend_weight: 1,
    outright_win_score_blend: 0,
    outright_live_score_placement_nudge: false,
    outrights_odds_format: "percent",
    matchups_odds_format: "decimal",
    players,
    props,
    outrights: {},
    matchups: {},
  };
}
