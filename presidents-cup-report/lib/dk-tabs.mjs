/** DraftKings Presidents Cup tournament subcategory tab order. */
export const DK_TABS = [
  { id: "outrights", label: "Outrights" },
  { id: "matchups", label: "Matchups" },
  { id: "hole", label: "Hole Props" },
  { id: "player-props", label: "Player Props" },
  { id: "top-scorer", label: "Top Scorer" },
  { id: "event-props", label: "Event Props" },
  { id: "round-props", label: "Round Props" },
  { id: "usa-props", label: "USA Props" },
  { id: "int.-props", label: "INT. Props" },
  { id: "team-props", label: "Team Props" },
  { id: "correct-score", label: "Correct Score" },
];

/** Scraped but folded into other tabs — not shown separately in the dashboard. */
export const DK_SKIP_TABS = new Set(["popular", "winner"]);

export const DK_SUBCATEGORIES = [
  "popular",
  "outrights",
  "winner",
  ...DK_TABS.map((t) => t.id),
];
