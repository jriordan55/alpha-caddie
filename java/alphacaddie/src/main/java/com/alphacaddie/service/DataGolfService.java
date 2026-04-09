package com.alphacaddie.service;

import com.alphacaddie.config.DataGolfProperties;
import com.alphacaddie.math.OuMath;
import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class DataGolfService {

    private static final double DEFAULT_PAR = 72;
    private static final double RAW_ROUND_SD = 2.75;

    private final WebClient dataGolfWebClient;
    private final DataGolfProperties props;

    public DataGolfService(WebClient dataGolfWebClient, DataGolfProperties props) {
        this.dataGolfWebClient = dataGolfWebClient;
        this.props = props;
    }

    private static boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }

    public boolean hasApiKey() {
        return !isBlank(props.getApiKey());
    }

    public JsonNode getFieldUpdates() {
        if (!hasApiKey()) return null;
        try {
            return dataGolfWebClient.get()
                    .uri(uriBuilder -> uriBuilder.path("/field-updates")
                            .queryParam("tour", "pga")
                            .queryParam("file_format", "json")
                            .queryParam("key", props.getApiKey())
                            .build())
                    .retrieve()
                    .bodyToMono(JsonNode.class)
                    .block(Duration.ofSeconds(45));
        } catch (WebClientResponseException e) {
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    public JsonNode getSkillRatings() {
        if (!hasApiKey()) return null;
        try {
            return dataGolfWebClient.get()
                    .uri(uriBuilder -> uriBuilder.path("/preds/skill-ratings")
                            .queryParam("display", "value")
                            .queryParam("file_format", "json")
                            .queryParam("key", props.getApiKey())
                            .build())
                    .retrieve()
                    .bodyToMono(JsonNode.class)
                    .block(Duration.ofSeconds(45));
        } catch (WebClientResponseException e) {
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    public JsonNode getMatchups(String market) {
        if (!hasApiKey()) return null;
        try {
            return dataGolfWebClient.get()
                    .uri(uriBuilder -> uriBuilder.path("/betting-tools/matchups")
                            .queryParam("tour", "pga")
                            .queryParam("market", market)
                            .queryParam("odds_format", "decimal")
                            .queryParam("file_format", "json")
                            .queryParam("key", props.getApiKey())
                            .build())
                    .retrieve()
                    .bodyToMono(JsonNode.class)
                    .block(Duration.ofSeconds(45));
        } catch (WebClientResponseException e) {
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    public JsonNode getOutrights(String market) {
        if (!hasApiKey()) return null;
        try {
            return dataGolfWebClient.get()
                    .uri(uriBuilder -> uriBuilder.path("/betting-tools/outrights")
                            .queryParam("tour", "pga")
                            .queryParam("market", market)
                            .queryParam("odds_format", "percent")
                            .queryParam("file_format", "json")
                            .queryParam("key", props.getApiKey())
                            .build())
                    .retrieve()
                    .bodyToMono(JsonNode.class)
                    .block(Duration.ofSeconds(45));
        } catch (WebClientResponseException e) {
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    /** dg_id -> sg_total (or total/overall). */
    public Map<Integer, Double> parseSkillByDgId(JsonNode root) {
        Map<Integer, Double> map = new HashMap<Integer, Double>();
        if (root == null || root.isNull()) return map;
        JsonNode arr = root.path("players");
        if (!arr.isArray()) arr = root;
        if (!arr.isArray()) return map;
        for (JsonNode row : arr) {
            int dg = row.path("dg_id").asInt(0);
            if (dg <= 0) continue;
            double sg = Double.NaN;
            if (row.has("sg_total")) sg = row.get("sg_total").asDouble();
            else if (row.has("total")) sg = row.get("total").asDouble();
            else if (row.has("overall")) sg = row.get("overall").asDouble();
            if (Double.isFinite(sg)) map.put(dg, sg);
        }
        return map;
    }

    public String eventName(JsonNode fieldRoot) {
        if (fieldRoot == null) return "";
        return fieldRoot.path("event_name").asText("");
    }

    public String courseName(JsonNode fieldRoot) {
        if (fieldRoot == null) return "";
        return fieldRoot.path("course_name").asText("");
    }

    public List<PlayerProjection> buildProjections(JsonNode fieldRoot, Map<Integer, Double> skillByDg, int roundNum) {
        List<PlayerProjection> out = new ArrayList<PlayerProjection>();
        if (fieldRoot == null) return out;
        JsonNode field = fieldRoot.path("field");
        if (!field.isArray()) return out;
        for (JsonNode p : field) {
            int dg = p.path("dg_id").asInt(0);
            String name = text(p, "player_name", "name");
            if (isBlank(name)) continue;
            String country = p.path("country").asText("");
            Double muObj = skillByDg.get(dg);
            double muSg = muObj != null ? muObj : 0.0;
            double scoreToPar = -muSg;
            double totalScore = DEFAULT_PAR + scoreToPar;
            double roundSd = RAW_ROUND_SD;
            Counts c = imputeCounts(muSg);
            String tt = teetimeForRound(p, roundNum);
            out.add(new PlayerProjection(dg, name, country, muSg, totalScore, roundSd, scoreToPar,
                    c.birdies, c.pars, c.bogeys, c.gir, c.fairways, tt));
        }
        out.sort((a, b) -> Double.compare(a.getTotalScore(), b.getTotalScore()));
        return out;
    }

    private static String text(JsonNode n, String... keys) {
        for (String k : keys) {
            if (n.has(k) && !n.get(k).isNull()) {
                String s = n.get(k).asText("").trim();
                if (!isBlank(s)) return s;
            }
        }
        return "";
    }

    private static final List<String> TEE_SENTINELS = Arrays.asList("na", "n/a", "--", "none", "null");

    private static String teetimeForRound(JsonNode p, int rn) {
        String k1 = "r" + rn + "_teetime";
        String k2 = "R" + rn + "_teetime";
        List<String> keys = Arrays.asList(k1, k2, "r" + rn + "_tee_time");
        for (String k : keys) {
            if (p.has(k)) {
                String s = p.get(k).asText("").trim();
                if (!isBlank(s) && !TEE_SENTINELS.contains(s.toLowerCase())) return s;
            }
        }
        return "";
    }

    private static final class Counts {
        final double birdies;
        final double pars;
        final double bogeys;
        final double gir;
        final double fairways;

        Counts(double birdies, double pars, double bogeys, double gir, double fairways) {
            this.birdies = birdies;
            this.pars = pars;
            this.bogeys = bogeys;
            this.gir = gir;
            this.fairways = fairways;
        }
    }

    /** Same shape as R {@code impute_counts_from_mu_sg} (raw projections). */
    private static Counts imputeCounts(double muSg) {
        double stp = Math.max(-8, Math.min(8, -muSg));
        double eagles = Math.max(0, 0.15 - 0.02 * stp);
        double birdies = Math.max(0.5, 3.8 - 0.45 * stp);
        double bogeys = Math.max(0.5, 2.6 + 0.5 * stp);
        double doubles = Math.max(0.1, 0.35 + 0.05 * stp);
        double pars = Math.max(0.2, 18 - eagles - birdies - bogeys - doubles);
        double s = eagles + birdies + pars + bogeys + doubles;
        double k = 18 / s;
        double stpVec = -muSg;
        double gir = Math.max(6, Math.min(16, 11.5 - 0.25 * stpVec));
        int nFw = 14;
        double fairways = Math.max(4, Math.min(nFw, 0.55 * nFw - 0.15 * stpVec));
        return new Counts(birdies * k, pars * k, bogeys * k, gir, fairways);
    }

    public List<OuTableRow> buildOuRows(List<PlayerProjection> players, String stat) {
        int[] lines = linesForStat(stat);
        List<OuTableRow> rows = new ArrayList<OuTableRow>();
        for (PlayerProjection pl : players) {
            String golfer = flagSpan(pl.getCountry()) + "<span class=\"golfer-name\">" + escapeHtml(displayName(pl.getPlayerName())) + "</span>";
            List<String> cells = new ArrayList<String>();
            String median;
            if ("Total score".equals(stat)) {
                median = String.format("%.2f", pl.getTotalScore());
                for (int L : lines) {
                    cells.add(OuMath.modelTotalScoreOuHtml(pl.getTotalScore(), pl.getRoundSd(), L));
                }
            } else {
                double lam = lambdaForStat(stat, pl);
                median = String.valueOf((int) Math.round(medianPoisson(lam)));
                for (int L : lines) {
                    cells.add(OuMath.modelCountOuHtml(lam, L));
                }
            }
            String tee = isBlank(pl.getTeetime()) ? "—" : pl.getTeetime();
            rows.add(new OuTableRow(golfer, median, cells, tee, pl.getTotalScore()));
        }
        rows.sort((a, b) -> Double.compare(a.getSortKey(), b.getSortKey()));
        return rows;
    }

    private static int[] linesForStat(String stat) {
        if ("Birdies".equals(stat) || "Bogeys".equals(stat)) return range(0, 6);
        if ("Pars".equals(stat) || "GIR".equals(stat) || "Fairways hit".equals(stat)) return range(5, 15);
        return range(64, 74);
    }

    private static double lambdaForStat(String stat, PlayerProjection pl) {
        if ("Birdies".equals(stat)) return pl.getBirdies();
        if ("Pars".equals(stat)) return pl.getPars();
        if ("Bogeys".equals(stat)) return pl.getBogeys();
        if ("GIR".equals(stat)) return pl.getGir();
        if ("Fairways hit".equals(stat)) return pl.getFairways();
        return pl.getBirdies();
    }

    private static int medianPoisson(double lambda) {
        if (!Double.isFinite(lambda) || lambda <= 0) return 0;
        int k = 0;
        double cdf = Math.exp(-lambda);
        double target = 0.5;
        while (cdf < target && k < 100) {
            k++;
            cdf += Math.exp(-lambda) * Math.pow(lambda, k) / factorial(k);
        }
        return k;
    }

    private static double factorial(int n) {
        if (n <= 1) return 1;
        double r = 1;
        for (int i = 2; i <= n; i++) r *= i;
        return r;
    }

    private static int[] range(int a, int bInclusive) {
        int[] x = new int[bInclusive - a + 1];
        for (int i = 0; i < x.length; i++) x[i] = a + i;
        return x;
    }

    public static String displayName(String raw) {
        if (raw == null || !raw.contains(",")) return raw == null ? "" : raw.trim();
        String[] p = raw.split(",\\s*", 2);
        return p.length >= 2 ? (p[1].trim() + " " + p[0].trim()) : raw.trim();
    }

    private static String escapeHtml(String s) {
        if (s == null) return "";
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;");
    }

    private static String flagSpan(String country) {
        if (isBlank(country)) return "";
        String iso = countryIso2(country.trim().toUpperCase());
        if (iso.isEmpty()) return "";
        return "<img src=\"https://flagcdn.com/w40/" + iso + ".png\" height=\"12\" alt=\"\" style=\"vertical-align:middle;border-radius:2px\"/> ";
    }

    private static String countryIso2(String c) {
        if ("USA".equals(c) || "UNITED STATES".equals(c)) return "us";
        if ("ENG".equals(c) || "SCO".equals(c) || "WAL".equals(c) || "NIR".equals(c) || "GBR".equals(c)) return "gb";
        if ("IRL".equals(c)) return "ie";
        if ("AUS".equals(c)) return "au";
        if ("KOR".equals(c)) return "kr";
        if ("JPN".equals(c)) return "jp";
        if ("CAN".equals(c)) return "ca";
        if ("RSA".equals(c) || "SOUTH AFRICA".equals(c)) return "za";
        return c.length() == 2 ? c.toLowerCase() : "";
    }

    public static final class PlayerProjection {
        private final int dgId;
        private final String playerName;
        private final String country;
        private final double muSg;
        private final double totalScore;
        private final double roundSd;
        private final double scoreToPar;
        private final double birdies;
        private final double pars;
        private final double bogeys;
        private final double gir;
        private final double fairways;
        private final String teetime;

        public PlayerProjection(int dgId, String playerName, String country, double muSg, double totalScore,
                double roundSd, double scoreToPar, double birdies, double pars, double bogeys,
                double gir, double fairways, String teetime) {
            this.dgId = dgId;
            this.playerName = playerName;
            this.country = country;
            this.muSg = muSg;
            this.totalScore = totalScore;
            this.roundSd = roundSd;
            this.scoreToPar = scoreToPar;
            this.birdies = birdies;
            this.pars = pars;
            this.bogeys = bogeys;
            this.gir = gir;
            this.fairways = fairways;
            this.teetime = teetime;
        }

        public int getDgId() { return dgId; }
        public String getPlayerName() { return playerName; }
        public String getCountry() { return country; }
        public double getMuSg() { return muSg; }
        public double getTotalScore() { return totalScore; }
        public double getRoundSd() { return roundSd; }
        public double getScoreToPar() { return scoreToPar; }
        public double getBirdies() { return birdies; }
        public double getPars() { return pars; }
        public double getBogeys() { return bogeys; }
        public double getGir() { return gir; }
        public double getFairways() { return fairways; }
        public String getTeetime() { return teetime; }

        public String getPrettyName() {
            return DataGolfService.displayName(playerName);
        }
    }

    public static final class OuTableRow {
        private final String golferHtml;
        private final String median;
        private final List<String> lineCells;
        private final String teeTime;
        private final double sortKey;

        public OuTableRow(String golferHtml, String median, List<String> lineCells, String teeTime, double sortKey) {
            this.golferHtml = golferHtml;
            this.median = median;
            this.lineCells = lineCells;
            this.teeTime = teeTime;
            this.sortKey = sortKey;
        }

        public String getGolferHtml() { return golferHtml; }
        public String getMedian() { return median; }
        public List<String> getLineCells() { return lineCells; }
        public String getTeeTime() { return teeTime; }
        public double getSortKey() { return sortKey; }
    }
}
