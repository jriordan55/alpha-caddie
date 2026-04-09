package com.alphacaddie.math;

/**
 * Model O/U pricing (Normal total, Poisson counts) + juiced American pairs — same idea as the old R/Shiny app.
 */
public final class OuMath {

    private static final double OU_HOLD = 0.048;

    private OuMath() {}

    public static double decimalToAmerican(double decimal) {
        if (!Double.isFinite(decimal) || decimal <= 1) return Double.NaN;
        if (decimal >= 2) return (decimal - 1) * 100;
        return -100 / (decimal - 1);
    }

    public static String formatAmerican(double american) {
        if (!Double.isFinite(american)) return "—";
        long r = Math.round(american);
        return r > 0 ? "+" + r : String.valueOf(r);
    }

    public static double[] juicedAmericanOuPair(double pOver, double pUnder) {
        double po = pOver;
        double pu = pUnder;
        if (!Double.isFinite(po) || !Double.isFinite(pu)) return new double[] {Double.NaN, Double.NaN};
        double s = po + pu;
        if (s <= 1e-12) return new double[] {Double.NaN, Double.NaN};
        po /= s;
        pu /= s;
        po = Math.max(1e-8, Math.min(1 - 1e-8, po));
        pu = Math.max(1e-8, Math.min(1 - 1e-8, pu));
        double s2 = po + pu;
        po /= s2;
        pu /= s2;
        double invO = po * (1 + OU_HOLD);
        double invU = pu * (1 + OU_HOLD);
        double decO = 1 / invO;
        double decU = 1 / invU;
        return new double[] {decimalToAmerican(decO), decimalToAmerican(decU)};
    }

    public static String modelTotalScoreOuHtml(double mu, double sigma, int line) {
        if (!Double.isFinite(mu)) return "—";
        double sig = Double.isFinite(sigma) && sigma > 0.25 ? sigma : 2.75;
        sig = Math.max(sig, 0.35);
        double pUnder = normalCdf(line + 0.5, mu, sig);
        double pOver = 1 - pUnder;
        pOver = Math.max(1e-8, Math.min(1 - 1e-8, pOver));
        pUnder = Math.max(1e-8, Math.min(1 - 1e-8, pUnder));
        double[] am = juicedAmericanOuPair(pOver, pUnder);
        return "<div class=\"ou-prices\"><span class=\"ou-o\">O " + formatAmerican(am[0])
                + "</span><br/><span class=\"ou-u\">U " + formatAmerican(am[1]) + "</span></div>";
    }

    public static String modelCountOuHtml(double lambda, int line) {
        if (!Double.isFinite(lambda) || lambda < 0) return "—";
        int k = Math.max(0, (int) Math.floor(line));
        double pUnder = poissonCdf(k, lambda);
        if (!Double.isFinite(pUnder)) return "—";
        double pOver = 1 - pUnder;
        pOver = Math.max(1e-8, Math.min(1 - 1e-8, pOver));
        pUnder = Math.max(1e-8, Math.min(1 - 1e-8, pUnder));
        double[] am = juicedAmericanOuPair(pOver, pUnder);
        return "<div class=\"ou-prices\"><span class=\"ou-o\">O " + formatAmerican(am[0])
                + "</span><br/><span class=\"ou-u\">U " + formatAmerican(am[1]) + "</span></div>";
    }

    public static double normalCdf(double x, double mean, double sd) {
        double z = (x - mean) / (sd * Math.sqrt(2));
        return 0.5 * (1 + erf(z));
    }

    private static double erf(double x) {
        double sign = x < 0 ? -1 : 1;
        double ax = Math.abs(x);
        double a1 = 0.254829592;
        double a2 = -0.284496736;
        double a3 = 1.421413741;
        double a4 = -1.453152027;
        double a5 = 1.061405429;
        double p = 0.3275911;
        double t = 1 / (1 + p * ax);
        double y = 1 - (((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t) * Math.exp(-ax * ax);
        return sign * y;
    }

    public static double poissonCdf(int k, double lambda) {
        if (lambda <= 0) return k >= 0 ? 1 : 0;
        double sum = 0;
        for (int i = 0; i <= k; i++) {
            sum += Math.exp(-lambda) * Math.pow(lambda, i) / factorial(i);
        }
        return Math.min(1, sum);
    }

    private static double factorial(int n) {
        if (n <= 1) return 1;
        double r = 1;
        for (int i = 2; i <= n; i++) r *= i;
        return r;
    }
}
