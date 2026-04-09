package com.alphacaddie.time;

import java.time.DayOfWeek;
import java.time.ZoneId;
import java.time.ZonedDateTime;

/** Mirrors Shiny ou_display_round_auto (America/New_York, 9pm cutoffs). */
public final class DisplayRound {

    private DisplayRound() {}

    public static int currentRound(ZonedDateTime nowEt) {
        DayOfWeek d = nowEt.getDayOfWeek();
        double hour = nowEt.getHour() + nowEt.getMinute() / 60.0 + nowEt.getSecond() / 3600.0;
        boolean after9 = hour >= 21;

        if (d == DayOfWeek.SUNDAY && after9) return 1;
        if (d.getValue() >= DayOfWeek.MONDAY.getValue() && d.getValue() <= DayOfWeek.WEDNESDAY.getValue()) return 1;
        if (d == DayOfWeek.THURSDAY && !after9) return 1;
        if (d == DayOfWeek.THURSDAY) return 2;
        if (d == DayOfWeek.FRIDAY && !after9) return 2;
        if (d == DayOfWeek.FRIDAY) return 3;
        if (d == DayOfWeek.SATURDAY && !after9) return 3;
        if (d == DayOfWeek.SATURDAY) return 4;
        if (d == DayOfWeek.SUNDAY) return 4;
        return 1;
    }

    public static int currentRound() {
        return currentRound(ZonedDateTime.now(ZoneId.of("America/New_York")));
    }

    public static String roundLabel(int r) {
        switch (r) {
            case 1: return "R1 — Thursday";
            case 2: return "R2 — Friday";
            case 3: return "R3 — Saturday";
            case 4: return "R4 — Sunday";
            default: return "R" + r;
        }
    }
}
