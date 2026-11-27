/**
 * Betsperts Rabbit Hole "Filter by Conditions" catalog (UI labels + API field mapping).
 * Multi-select condition fields must be sent as string arrays on rabbit_hole/load.
 */

/** @typedef {{ key: string, label: string, apiField: string, asArray?: boolean, values: { value: string, label?: string, apiValue?: string }[] }} FilterDim */

/** Screenshot Filter-by-Conditions dimensions + API-backed extras (region/ctype). */
export const RH_CONDITION_FILTERS = Object.freeze([
  {
    key: "scoring_conditions",
    label: "Scoring Conditions",
    apiField: "condition",
    asArray: true,
    values: ["Very Easy", "Easy", "Average", "Difficult", "Very Difficult"].map((v) => ({ value: v })),
  },
  {
    key: "course_length",
    label: "Course Length",
    apiField: "clength",
    asArray: true,
    values: ["Very Short", "Short", "Average", "Long", "Very Long"].map((v) => ({ value: v })),
  },
  {
    key: "field_strength",
    label: "Field Strength",
    apiField: "fieldStrength",
    asArray: true,
    values: ["Very Weak", "Weak", "Average", "Strong", "Very Strong"].map((v) => ({ value: v })),
  },
  {
    key: "round_number",
    label: "Round Number",
    apiField: "round",
    asArray: false,
    // API `round` is a rolling last-N window (1/2/3/4 = last N rounds), not tournament R1–R4.
    // Store UI labels; pulls use apiValue for last-N semantics when requested.
    values: [
      { value: "Round 1", apiValue: "1", note: "API rolling last-1 (not tournament R1)" },
      { value: "Round 2", apiValue: "2", note: "API rolling last-2" },
      { value: "Round 3", apiValue: "3", note: "API rolling last-3" },
      { value: "Round 4", apiValue: "4", note: "API rolling last-4" },
    ],
    pull: false,
  },
  {
    key: "par",
    label: "Par",
    apiField: "par",
    asArray: true,
    values: ["70", "71", "72", "73"].map((v) => ({ value: v })),
  },
  {
    key: "event_type",
    label: "Event Type",
    apiField: "cut",
    asArray: true,
    values: [
      { value: "Major", apiValue: "Major" },
      { value: "Signature", apiValue: "Signature" },
      { value: "No Cut", apiValue: "No-Cut" },
      { value: "36 hole Cut", apiValue: "36-Hole Cut", pull: false }, // UI label; API often empty
      { value: "54 hole Cut", apiValue: "54-Hole Cut", pull: false },
      { value: "FedEx Cup Playoffs", apiValue: "FedEx Cup Playoffs" },
      { value: "Olympics", apiValue: "Olympics" },
    ],
  },
  {
    key: "majors",
    label: "Majors",
    apiField: "tournament",
    asArray: true,
    values: [
      "Masters Tournament",
      "PGA Championship",
      "U.S. Open",
      "The Open Championship",
    ].map((v) => ({ value: v })),
  },
  {
    key: "event_season",
    label: "Event Season",
    apiField: "eventSeason",
    asArray: true,
    values: ["Aloha", "West Coast", "Florida", "Spring", "Summer", "Playoffs", "Fall"].map((v) => ({
      value: v,
    })),
  },
  {
    key: "field_size",
    label: "Field Size",
    apiField: "fieldSize",
    asArray: true,
    values: ["Large", "Intermediate", "Small"].map((v) => ({ value: v })),
  },
  {
    key: "water_danger",
    label: "Water Danger",
    apiField: "waterDanger",
    asArray: true,
    values: ["Low", "Average", "High"].map((v) => ({ value: v })),
  },
  {
    key: "greens_surface",
    label: "Greens Surface",
    apiField: "green",
    asArray: true,
    values: ["Bent", "Bermuda", "Poa", "Poa Trivialis", "Bent/Poa", "Paspalum", "Native"].map((v) => ({
      value: v,
    })),
  },
  {
    key: "greens_size",
    label: "Greens Size",
    apiField: "greenSize",
    asArray: true,
    values: ["Large", "Average", "Small"].map((v) => ({ value: v })),
  },
  {
    key: "greens_speed",
    label: "Greens Speed",
    apiField: "speed",
    asArray: true,
    values: ["Fast", "Average", "Slow"].map((v) => ({ value: v })),
  },
  {
    key: "bunker_count",
    label: "Bunker #",
    apiField: "bunkerDanger",
    asArray: true,
    values: ["Low", "Average", "High"].map((v) => ({ value: v })),
  },
  {
    key: "fairway_surface",
    label: "Fairway Surface",
    apiField: "fairwaySurface",
    asArray: true,
    values: ["Bent", "Bermuda", "Rye", "Fescue Mix", "Paspalum", "Zoysia", "Kikuyu"].map((v) => ({
      value: v,
    })),
  },
  {
    key: "rough_surface",
    label: "Rough Surface",
    apiField: "roughSurface",
    asArray: true,
    values: ["Bluegrass", "Bermuda", "Rye", "Fescue", "Paspalum", "Zoysia", "Kikuyu"].map((v) => ({
      value: v,
    })),
  },
  {
    key: "rough_length",
    label: "Rough Length",
    apiField: "rough",
    asArray: true,
    values: ["Long", "Average", "Short"].map((v) => ({ value: v })),
  },
  {
    key: "elevation",
    label: "Elevation",
    apiField: "elevation",
    asArray: true,
    values: ["Sea Level", "Average", "High"].map((v) => ({ value: v })),
  },
  {
    key: "architect",
    label: "Architect / Redesigner",
    apiField: "architect",
    asArray: true,
    values: [
      "Albert Tillinghast",
      "Arnold Palmer",
      "Coore/Crenshaw",
      "Dick Wilson",
      "Donald Ross",
      "Gil Hanse",
      "Greg Norman",
      "Keith Foster",
      "Tom Weiskopf",
      "Harry Colt",
      "Jack Nicklaus",
      "Pete Dye",
      "Rees Jones",
      "Robert Trent Jones",
      "Seth Raynor",
      "Tom Doak",
      "Tom Fazio",
      "William Bell",
    ].map((v) => ({ value: v })),
  },
  {
    key: "ott_conditions",
    label: "OTT Conditions",
    apiField: "gainOTT",
    asArray: true,
    values: ["Very Easy", "Easy", "Average", "Difficult"].map((v) => ({ value: v })),
  },
  {
    key: "ott_club",
    label: "OTT Club",
    apiField: "OTTClub",
    asArray: true,
    values: ["Driver Heavy", "Less Than Driver", "Mixed"].map((v) => ({ value: v })),
  },
  {
    key: "fairway_accuracy",
    label: "Fairway Accuracy",
    apiField: "fwyAccuracy",
    asArray: true,
    values: ["Easy", "Average", "Difficult"].map((v) => ({ value: v })),
  },
  {
    key: "missed_fairway_penalty",
    label: "Missed Fwy Penalty",
    apiField: "missedFwyPenalty",
    asArray: true,
    values: ["Low", "Average", "High"].map((v) => ({ value: v })),
  },
  {
    key: "rough_penalty",
    label: "Rough Penalty",
    apiField: "roughPenalty",
    asArray: true,
    values: ["Low", "Average", "High"].map((v) => ({ value: v })),
  },
  {
    key: "tee_time",
    label: "Tee Time (AM/PM)",
    apiField: "teeTime",
    asArray: true,
    values: [
      { value: "AM", label: "AM" },
      { value: "PM", label: "PM" },
    ],
  },
  {
    key: "region",
    label: "Region",
    apiField: "region",
    asArray: true,
    valuesFrom: "regions",
  },
  {
    key: "course_type",
    label: "Course Type",
    apiField: "ctype",
    asArray: true,
    valuesFrom: "types",
  },
]);

export function buildFilterCatalogPayload({ regions = [], types = [], courses = [] } = {}) {
  const dimensions = RH_CONDITION_FILTERS.map((dim) => {
    let values = dim.values || [];
    if (dim.valuesFrom === "regions") {
      values = (regions || []).map((v) => ({ value: String(v) }));
    } else if (dim.valuesFrom === "types") {
      values = (types || []).map((v) => ({ value: String(v) }));
    }
    return {
      key: dim.key,
      label: dim.label,
      apiField: dim.apiField,
      asArray: dim.asArray !== false,
      pull: dim.pull !== false,
      values,
    };
  });
  return {
    generatedAt: new Date().toISOString(),
    source: "betsperts Filter by Conditions UI + rabbit_hole/load field map",
    dimensions,
    courses: (courses || []).map((c) =>
      typeof c === "string"
        ? { id: "", name: c }
        : { id: String(c.id || c.CourseNum || ""), name: String(c.name || c.CourseName || "") },
    ),
  };
}

/** Expand catalog into single-value pull jobs (skip pull:false). */
export function expandFilterPullValues(catalog) {
  const out = [];
  for (const dim of catalog.dimensions || []) {
    if (dim.pull === false) continue;
    for (const v of dim.values || []) {
      if (v.pull === false) continue;
      const apiValue = v.apiValue != null ? String(v.apiValue) : String(v.value);
      if (!apiValue) continue;
      out.push({
        dimKey: dim.key,
        dimLabel: dim.label,
        apiField: dim.apiField,
        asArray: dim.asArray !== false,
        value: String(v.value),
        apiValue,
      });
    }
  }
  return out;
}

/** Build rabbit_hole/load override for one filter value. */
export function filterValueToOverrides(filterVal) {
  const o = {};
  if (filterVal.asArray === false) {
    o[filterVal.apiField] = filterVal.apiValue;
  } else {
    o[filterVal.apiField] = [filterVal.apiValue];
  }
  return o;
}
