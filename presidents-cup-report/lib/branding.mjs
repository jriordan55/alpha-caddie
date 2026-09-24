/** Official Presidents Cup 2026 branding — colors & assets from presidentscup.com */

export const PC_BASE = "https://www.presidentscup.com";

export const THEME = {
  red: "#D9000A",
  redBright: "#ED0000",
  gold: "#F5A81C",
  goldDark: "#D67A27",
  green: "#006039",
  black: "#19191A",
  gray: "#63656A",
  grayLight: "#E3E3E3",
  white: "#FFFFFF",
  offWhite: "#F2F2F2",
  usa: "#D9000A",
  usaBg: "#1a0506",
  int: "#0084FF",
  intBg: "#061428",
  intAccent: "#006039",
};

export const ASSETS = {
  hero: `${PC_BASE}/media_1e779a42e5fa047dc30056d3b05361c5cfc40755e.jpg?width=1600&format=jpg&optimize=medium`,
  course: `${PC_BASE}/media_1e2ea93f51ff44c067f6e2449335e27a52f5dc59a.jpg?width=1200&format=jpg&optimize=medium`,
  usaBadge: `${PC_BASE}/media_104b23ce8d42046c40e18d3fbe3c999d423191d30.png?width=400&format=png&optimize=medium`,
  intBadge: `${PC_BASE}/media_1dfaefda33b4a39db3e2def94c5813bde8ea8afac.png?width=400&format=png&optimize=medium`,
};

/** Bundled logos in public/assets/. */
export const LOCAL_LOGOS = {
  /** Presidents Cup event wordmark */
  event: "/assets/pc-logo-full.png",
  /** International team logo (black/gold trophy) */
  int: "/assets/pc-logo-icon.png",
  usaFlag: "/assets/usa-flag.png",
  usaBadge: "/assets/usa-badge.png",
};

/** Official roster headshots from presidentscup.com (2026 teams). */
export const HEADSHOT_MAP = {
  "scheffler, scottie": "media_1fda73d8d1c4115eff80b0c1ab3b59d6e38af89f1.png",
  "gotterup, chris": "media_191f153971c41f0934f9eb4eeb09f3355d678c28d.png",
  "schauffele, xander": "media_141835fb563cb8a0011fadfd90ca0f9f24d5d6fd5.png",
  "young, cameron": "media_15a70a45965267545d13365eb00e2d84e19520fdc.png",
  "burns, sam": "media_1f878d4b8a3ad38aa7dd04bc7f67fd359f1abeaa6.png",
  "cantlay, patrick": "media_109b0faa61c3b06408090c5769ceb88970ec5dd88.png",
  "morikawa, collin": "media_15020cd0e2b08e8d853cc622f1af27cd78b67ca27.png",
  "bridgeman, jacob": "media_1b40f79a0917c61a7e5fb897b9c14102bdedbb4c2.png",
  "clark, wyndham": "media_125045bc98a34353fec6969fd2db2ba3451ac7f09.png",
  "thomas, justin": "media_13428103d025ec48c6186817cef5c95f989e4bc73.png",
  "henley, russell": "media_18c97fbb7926a61a7a69bab358e5aff5b7e873049.png",
  "koivun, jackson": "media_19983ab5a85c6c2a26f3f3b2d1bdedd7d2a6a7599.png",
  "kim, si woo": "media_15019d5e51bea1b760e93edb8143e47281ea282e7.png",
  "kim, tom": "media_164665a3a02ff61537e0f37592c0a0dead664b103.png",
  "scott, adam": "media_196c0152dab120091bc98bc927ae2cc9c6465c190.png",
  "matsuyama, hideki": "media_14ee88e30bc2aa3cffd901f39dadcc7bbf0a48f81.png",
  "lee, min woo": "media_1b7bb4e00727962fea441d8328ce822d8dbe7ca39.png",
  "im, sungjae": "media_171201ade4a49870b63a91ec0e516ae9766da7db6.png",
  "fox, ryan": "media_14e8ab87c91dd32770f5ca4bc4fe98cb731cfdd8d.png",
  "conners, corey": "media_1abb45abac74bbf71cf7a78cb88e90d168cbb34ed.png",
  "hisatsune, ryo": "media_1d8a4c84669f91374346cdb6046228b08a4320c19.png",
  "taylor, nick": "media_10d3070affab20175ad5d6abc6ae2d0ae68972bad.png",
  "bezuidenhout, christiaan": "media_184588751fabc0839d23e6b581be1bf924bc7d79b.png",
  "echavarria, nico": "media_17280a6d6b2a9cd7621b67187b66dd852673f3092.png",
};

function normKey(name) {
  return String(name || "")
    .toLowerCase()
    .replace(/\s+/g, " ")
    .trim();
}

/** DK / media spelling variants → roster key. */
const NAME_ALIASES = {
  "nicolas echavarria": "echavarria, nico",
  "nico echavarria": "echavarria, nico",
};

/** Normalize any roster/DK name to the HEADSHOT_MAP key ("last, first"). */
export function playerKey(name) {
  const k = normKey(name);
  if (NAME_ALIASES[k]) return NAME_ALIASES[k];
  if (HEADSHOT_MAP[k]) return k;

  if (k.includes(",")) {
    const [last, first] = k.split(",").map((x) => x.trim());
    const reversed = `${last}, ${first}`;
    if (HEADSHOT_MAP[reversed]) return reversed;
    return k;
  }

  const parts = k.split(" ").filter(Boolean);
  if (parts.length >= 2) {
    const last = parts[parts.length - 1];
    const first = parts.slice(0, -1).join(" ");
    const rosterKey = `${last}, ${first}`;
    if (HEADSHOT_MAP[rosterKey]) return rosterKey;
  }

  return k;
}

export function playerHeadshotUrl(name, manifest) {
  const key = playerKey(name);
  if (manifest?.headshots?.[key]) return manifest.headshots[key];
  const file = HEADSHOT_MAP[key];
  if (!file) return null;
  return `${PC_BASE}/${file}?width=320&format=png&optimize=medium`;
}

/** Extract player name from DK market titles like "Adam Scott Total Points". */
export function playerFromMarket(market) {
  const m = String(market || "").match(
    /^(.+?)\s+(to win a point|total points|most holes|to win the most holes|to play most holes|most birdies)/i,
  );
  return m ? m[1].trim() : null;
}

export function headshotForSelection(selection, market, manifest) {
  const sel = String(selection || "").trim();
  if (/^(usa|international|tie|yes|no)$/i.test(sel)) {
    const fromMarket = playerFromMarket(market);
    if (fromMarket) return playerHeadshotUrl(fromMarket, manifest);
    return null;
  }
  if (/^[\d.]+$/.test(sel) || /^over\s|^under\s/i.test(sel)) {
    const fromMarket = playerFromMarket(market);
    if (fromMarket) return playerHeadshotUrl(fromMarket, manifest);
    return null;
  }
  return playerHeadshotUrl(sel, manifest);
}

export function badgeUrl(team, manifest) {
  if (team === "USA") {
    return (
      manifest?.badges?.["usa-flag"] ||
      LOCAL_LOGOS.usaFlag ||
      manifest?.badges?.["usa-badge"] ||
      LOCAL_LOGOS.usaBadge
    );
  }
  return (
    manifest?.badges?.["pc-logo-icon"] ||
    manifest?.badges?.["int-logo"] ||
    LOCAL_LOGOS.int
  );
}

export function eventLogoUrl(manifest) {
  return manifest?.badges?.["pc-logo-full"] || LOCAL_LOGOS.event;
}

export function displayName(name) {
  const s = String(name || "").trim();
  if (!s.includes(",")) return s;
  const [last, first] = s.split(",").map((x) => x.trim());
  return `${first} ${last}`;
}
