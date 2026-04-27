#!/usr/bin/env python3
import csv
import os
import re
from collections import defaultdict
from datetime import datetime


def repo_root() -> str:
    env = (os.environ.get("GOLF_MODEL_DIR") or "").strip()
    if env:
        return os.path.abspath(env)
    return os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))


ROOT = repo_root()
SHOTS_CSV = os.path.join(ROOT, "data", "all_shots_2022_2026.csv")
PLAYER_MAP_CSV = os.path.join(ROOT, "data", "pga_datagolf_player_map.csv")
TOURNEY_MAP_CSV = os.path.join(ROOT, "data", "pga_tournament_course_map.csv")
HIST_CSV = os.path.join(ROOT, "data", "historical_rounds_all.csv")
OUT_CSV = os.path.join(ROOT, "data", "all_shots_2022_2026_round_fairways_gir_putts.csv")


def norm_event_name(s: str) -> str:
    s = (s or "").strip().lower()
    s = s.replace("&", " and ")
    s = re.sub(r"[^a-z0-9]+", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def parse_year_from_tournament_id(tid: str):
    m = re.match(r"^[A-Za-z](\d{4})", (tid or "").strip())
    if not m:
        return None
    return int(m.group(1))


def to_code_indicates_fairway(to_code: str) -> bool:
    tc = (to_code or "").strip().upper()
    if not tc or tc == "HOLE":
        return False
    if "FAIR" in tc:
        return True
    return tc in {"ELF", "ERF", "OFW", "ECF", "XFW"}


def to_code_indicates_green(to_code: str) -> bool:
    tc = (to_code or "").strip().upper()
    if not tc:
        return False
    if tc == "OGR":
        return True
    return "GREEN" in tc


def hole_gir_fairway(par: int, to_codes):
    fairway_hit = None
    if par in (4, 5) and len(to_codes) >= 1:
        fairway_hit = 1 if to_code_indicates_fairway(to_codes[0]) else 0
    req = 1 if par == 3 else 2 if par == 4 else 3 if par == 5 else None
    gir = 0
    if req is not None and len(to_codes) >= req:
        gir = 1 if to_code_indicates_green(to_codes[req - 1]) else 0
    return fairway_hit, gir


def load_player_map():
    out = {}
    with open(PLAYER_MAP_CSV, newline="", encoding="utf-8", errors="ignore") as f:
        r = csv.DictReader(f)
        for row in r:
            pid = (row.get("pga_player_id") or "").strip()
            if not pid:
                continue
            name = (row.get("pga_display_name") or "").strip()
            if not name:
                name = (row.get("dg_player_name") or "").strip()
            out[pid] = name or pid
    return out


def load_tourney_map():
    out = {}
    with open(TOURNEY_MAP_CSV, newline="", encoding="utf-8", errors="ignore") as f:
        r = csv.DictReader(f)
        for row in r:
            tid = (row.get("tournament_id") or "").strip()
            if not tid:
                continue
            course = (row.get("course_name") or "").strip()
            tname = (row.get("tournament_name") or "").strip()
            year_raw = (row.get("year") or "").strip()
            year = int(year_raw) if year_raw.isdigit() else parse_year_from_tournament_id(tid)
            out[tid] = {
                "course": course,
                "tournament_name": tname,
                "year": year,
            }
    return out


def load_event_dates():
    # key: (normalized_event_name, year) -> date string (event_completed); first row wins.
    out = {}
    with open(HIST_CSV, newline="", encoding="utf-8", errors="ignore") as f:
        r = csv.DictReader(f)
        for row in r:
            yr_raw = (row.get("year") or "").strip()
            dt = (row.get("event_completed") or "").strip()
            if not yr_raw or not dt:
                continue
            try:
                yr = int(yr_raw)
            except ValueError:
                continue
            for raw_name in (row.get("event_name") or "", row.get("course_name") or ""):
                en = norm_event_name(raw_name)
                if not en:
                    continue
                key = (en, yr)
                if key not in out:
                    out[key] = dt
    return out


def resolve_event_date(event_dates, tname, shot_tname, year):
    """Match historical `event_completed` using course-map name and/or live shots file tournament_name."""
    if year is None:
        return ""
    for cand in (shot_tname, tname):
        if not cand:
            continue
        dt = event_dates.get((norm_event_name(cand), year), "")
        if dt:
            return dt
    return ""


def main():
    player_map = load_player_map()
    tourney_map = load_tourney_map()
    event_dates = load_event_dates()

    round_aggs = defaultdict(lambda: {"fairways": 0, "gir": 0, "putts": 0})
    tid_shot_tname = {}

    cur_hole_key = None
    cur_hole_par = None
    cur_to_codes = []

    def flush_hole():
        nonlocal cur_hole_key, cur_hole_par, cur_to_codes
        if cur_hole_key is None:
            return
        tid, pid, rnd, _hole = cur_hole_key
        rk = (tid, pid, rnd)
        fh, gir = hole_gir_fairway(cur_hole_par, cur_to_codes)
        if fh is not None:
            round_aggs[rk]["fairways"] += int(fh)
        round_aggs[rk]["gir"] += int(gir)
        cur_hole_key = None
        cur_hole_par = None
        cur_to_codes = []

    with open(SHOTS_CSV, newline="", encoding="utf-8", errors="ignore") as f:
        r = csv.DictReader(f)
        for row in r:
            tid = (row.get("tournament_id") or "").strip()
            pid = (row.get("player_id") or "").strip()
            rnd = (row.get("round") or "").strip()
            hole = (row.get("hole_number") or "").strip()
            if not tid or not pid or not rnd or not hole:
                continue

            tn = (row.get("tournament_name") or "").strip()
            if tn:
                tid_shot_tname[tid] = tn

            # Putts per round
            rk = (tid, pid, rnd)
            stroke_type = (row.get("stroke_type") or "").strip().upper()
            from_code = (row.get("from_location_code") or "").strip().upper()
            if stroke_type == "PUTT" or from_code == "OGR":
                round_aggs[rk]["putts"] += 1

            hk = (tid, pid, rnd, hole)
            if cur_hole_key is None:
                cur_hole_key = hk
                try:
                    cur_hole_par = int(float((row.get("par") or "0").strip() or "0"))
                except ValueError:
                    cur_hole_par = 0
                cur_to_codes = []
            elif hk != cur_hole_key:
                flush_hole()
                cur_hole_key = hk
                try:
                    cur_hole_par = int(float((row.get("par") or "0").strip() or "0"))
                except ValueError:
                    cur_hole_par = 0
                cur_to_codes = []

            cur_to_codes.append((row.get("to_location_code") or "").strip())

    flush_hole()

    rows = []
    for (tid, pid, rnd), vals in round_aggs.items():
        tmeta = tourney_map.get(tid, {})
        course = tmeta.get("course") or ""
        tname = tmeta.get("tournament_name") or ""
        year = tmeta.get("year")
        if year is None:
            year = parse_year_from_tournament_id(tid)
        shot_tname = tid_shot_tname.get(tid, "")
        date = resolve_event_date(event_dates, tname, shot_tname, year)
        evt_norm = norm_event_name(tname or shot_tname)
        golfer = player_map.get(pid, pid)
        rows.append(
            {
                "date": date,
                "year": year if year is not None else "",
                "evt_norm": evt_norm,
                "round": rnd,
                "course": course,
                "golfer": golfer,
                "fairways": vals["fairways"],
                "gir": vals["gir"],
                "putts": vals["putts"],
            }
        )

    def sort_key(x):
        d = x["date"]
        try:
            dt = datetime.strptime(d, "%m/%d/%Y")
        except Exception:
            dt = datetime.min
        try:
            yf = int(x["year"]) if str(x.get("year") or "").strip().isdigit() else 0
        except Exception:
            yf = 0
        try:
            r = int(x["round"])
        except Exception:
            r = 99
        return (dt, yf, x["course"], x["golfer"], r)

    rows.sort(key=sort_key)

    with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "date",
                "year",
                "evt_norm",
                "round",
                "course",
                "golfer",
                "fairways",
                "gir",
                "putts",
            ],
        )
        w.writeheader()
        w.writerows(rows)

    print(f"Wrote {len(rows)} rows -> {OUT_CSV}")


if __name__ == "__main__":
    main()

