#!/usr/bin/env python3
"""Quick-save pin sheets parsed via vision into the DB."""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

WEB = Path(__file__).resolve().parents[1] / "alpha-caddie-web"
SAVE = WEB / "scripts" / "save-pin-sheets-batch.mjs"
BASE = Path(__file__).resolve().parents[1] / "data" / "pin_locations_extracted"


def h(hole, depth, front, side, ps, hazard=False):
    return {
        "hole": hole,
        "green_depth_yds": depth,
        "pin_from_front_yds": front,
        "pin_from_side_yds": side,
        "pin_side": ps,
        "near_hazard": hazard,
    }


def sheet(name, course, date, rnd, event, holes):
    return {
        "course_name": course,
        "play_date": date,
        "round_num": rnd,
        "event_name_ref": event,
        "source_image": name,
        "_image_src": str(BASE / name),
        "holes": holes,
    }


SHEETS = [
    sheet(
        "IMG_9221.PNG",
        "Vidanta Vallarta",
        "2025-02-21",
        2,
        "Mexico Open at VidantaWorld",
        [
            h(1, 30, 24, 9, "L"),
            h(2, 31, 7, 9, "L"),
            h(3, 35, 21, 7, "R"),
            h(4, 34, 25, 11, "L"),
            h(5, 40, 25, 9, "R"),
            h(6, 46, 24, 4, "L"),
            h(7, 42, 31, 6, "R"),
            h(8, 40, 7, 5, "R"),
            h(9, 37, 18, 4, "R"),
            h(10, 35, 6, 5, "L"),
            h(11, 37, 21, 4, "R"),
            h(12, 41, 32, 6, "R"),
            h(13, 44, 35, 8, "R"),
            h(14, 35, 20, 6, "L"),
            h(15, 33, 22, 4, "L"),
            h(16, 32, 27, 9, "R"),
            h(17, 35, 8, 5, "L"),
            h(18, 25, 17, 9, "L"),
        ],
    ),
    sheet(
        "IMG_9223.PNG",
        "Vidanta Vallarta",
        "2025-02-23",
        4,
        "Mexico Open at VidantaWorld",
        [
            h(1, 30, 14, 6, "R"),
            h(2, 31, 14, 7, "L"),
            h(3, 35, 30, 7, "R"),
            h(4, 34, 27, 8, "R"),
            h(5, 40, 15, 4, "L"),
            h(6, 46, 41, 5, "L"),
            h(7, 42, 10, 4, "L"),
            h(8, 40, 35, 7, "R"),
            h(9, 37, 30, 4, "L"),
            h(10, 35, 27, 6, "R"),
            h(11, 37, 8, 5, "R"),
            h(12, 41, 36, 6, "L"),
            h(13, 44, 36, 5, "L"),
            h(14, 35, 7, 5, "L"),
            h(15, 33, 12, 4, "R"),
            h(16, 32, 25, 5, "R"),
            h(17, 32, 18, 5, "L"),
            h(18, 25, 18, 7, "R"),
        ],
    ),
    sheet(
        "IMG_9224.PNG",
        "PGA National Resort (The Champion Course)",
        "2025-02-27",
        1,
        "Cognizant Classic in The Palm Beaches",
        [
            h(1, 35, 13, 5, "R"),
            h(2, 39, 10, 4, "R"),
            h(3, 41, 18, 4, "R"),
            h(4, 26, 18, 7, "L"),
            h(5, 34, 6, 7, "R"),
            h(6, 33, 27, 6, "R"),
            h(7, 43, 36, 1, "L"),
            h(8, 34, 20, 4, "R"),
            h(9, 37, 26, 5, "L"),
            h(10, 35, 27, 3, "L"),
            h(11, 36, 5, 9, "L", True),
            h(12, 35, 30, 9, "L"),
            h(13, 35, 18, 4, "L"),
            h(14, 34, 25, 7, "R", True),
            h(15, 37, 18, 5, "R", True),
            h(16, 36, 8, 5, "R", True),
            h(17, 36, 8, 5, "L", True),
            h(18, 37, 16, 11, "R", True),
        ],
    ),
]


def main():
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False, encoding="utf-8") as tf:
        json.dump(SHEETS, tf, indent=2)
        path = tf.name
    r = subprocess.run(["node", str(SAVE), path], cwd=str(WEB))
    Path(path).unlink(missing_ok=True)
    sys.exit(r.returncode)


if __name__ == "__main__":
    main()
