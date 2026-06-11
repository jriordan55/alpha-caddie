#!/usr/bin/env python3
"""
Parse PGA Tour ShotLink pin sheet tweet screenshots via EasyOCR.
Writes batch JSON for save-pin-sheets-batch.mjs.

Usage:
  python scripts/import_pin_sheets_ocr.py
  python scripts/import_pin_sheets_ocr.py --limit 5
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
from datetime import datetime
from pathlib import Path

import easyocr
from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
EXTRACTED = ROOT / "data" / "pin_locations_extracted"
if not EXTRACTED.exists():
    EXTRACTED = ROOT / "data" / "pin_locations" / "_import_staging"
OUT_DIR = ROOT / "data" / "pin_locations" / "batches"
OUT_DIR.mkdir(parents=True, exist_ok=True)

MONTHS = {
    "january": 1, "february": 2, "march": 3, "april": 4, "may": 5, "june": 6,
    "july": 7, "august": 8, "september": 9, "october": 10, "november": 11, "december": 12,
}


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--limit", type=int, default=0)
    p.add_argument("--force", action="store_true")
    return p.parse_args()


def ocr_image(reader, path: Path) -> list[tuple]:
    img = Image.open(path).convert("RGB")
    w, h = img.size
    crop = img.crop((0, int(h * 0.12), w, int(h * 0.88)))
    import numpy as np
    return reader.readtext(np.array(crop), detail=1, paragraph=False)


def texts_flat(items) -> str:
    return " ".join(t[1] for t in items)


def parse_header(text: str) -> dict:
    out = {"course_name": "", "play_date": "", "round_num": 0, "event_name_ref": ""}
    low = text.lower()

    m = re.search(r"round\s*(\d)", text, re.I)
    if m:
        out["round_num"] = int(m.group(1))

    m = re.search(
        r"(monday|tuesday|wednesday|thursday|friday|saturday|sunday)[;,.\s]+([a-z]+)\s+(\d{1,2})[,.\s]+(\d{4})",
        low,
    )
    if m:
        mo = MONTHS.get(m.group(2), 0)
        if mo:
            out["play_date"] = f"{int(m.group(4)):04d}-{mo:02d}-{int(m.group(3)):02d}"

    # Course line: "... at TPC Toronto at Osprey Valley (North Course)"
    m = re.search(r"\bat\s+([A-Z0-9][^\n\r]{8,120}?)(?:\s+Round|\s+round|\s+DSHO|\s+Shot|\s+Each|\s*$)", text, re.I)
    if m:
        out["course_name"] = re.sub(r"\s+", " ", m.group(1)).strip(" .,")
    else:
        m = re.search(r"([A-Z][A-Za-z0-9&\s.'()-]{10,80}(?:Course|Club|Links|Resort|CC|G\.C\.))", text)
        if m:
            out["course_name"] = re.sub(r"\s+", " ", m.group(1)).strip()

    m = re.search(r"(?:first|second|third|fourth|\d+(?:st|nd|rd|th))\s+round\s+of\s+(?:the\s+)?(.+?)(?:\s+Round|\s+at\s+|\s+Thursday|\s+Friday|\s+Saturday|\s+Sunday|\s+Monday|\s+Tuesday|\s+Wednesday|$)", text, re.I)
    if m:
        out["event_name_ref"] = re.sub(r"\s+", " ", m.group(1)).strip(" .,")

    return out


def num_from(s: str) -> int | None:
    m = re.search(r"\d+", s)
    return int(m.group()) if m else None


def parse_holes_from_items(items, img_path: Path) -> list[dict]:
    """Heuristic hole parser from OCR boxes on cropped pin sheet."""
    img = Image.open(img_path).convert("RGB")
    w, h = img.size
    y0, y1 = int(h * 0.12), int(h * 0.88)
    crop_h = y1 - y0

    # 6 cols x 3 rows hole grid (approximate relative to crop)
    grid_top = y0 + int(crop_h * 0.18)
    grid_bottom = y0 + int(crop_h * 0.92)
    grid_left = int(w * 0.04)
    grid_right = int(w * 0.96)
    cols, rows = 6, 3
    cell_w = (grid_right - grid_left) / cols
    cell_h = (grid_bottom - grid_top) / rows

    holes = []
    for row in range(rows):
        for col in range(cols):
            hole_num = row * cols + col + 1
            cx0 = grid_left + col * cell_w
            cy0 = grid_top + row * cell_h
            cx1 = cx0 + cell_w
            cy1 = cy0 + cell_h
            cx, cy = (cx0 + cx1) / 2, (cy0 + cy1) / 2

            cell_items = []
            for bbox, txt, conf in items:
                xs = [p[0] for p in bbox]
                ys = [p[1] for p in bbox]
                bx, by = sum(xs) / len(xs), sum(ys) / len(ys)
                if cx0 <= bx <= cx1 and cy0 <= by <= cy1:
                    cell_items.append((by, bx, txt, conf))

            cell_items.sort(key=lambda t: (t[0], t[1]))
            nums = []
            pin_side = None
            for _, _, txt, conf in cell_items:
                t = txt.strip()
                if re.fullmatch(r"[LRl]", t):
                    pin_side = t.upper()
                if re.search(r"depth|green|hole|shotlink|each", t, re.I):
                    continue
                n = num_from(t)
                if n is not None and 0 < n <= 50:
                    nums.append(n)

            # Typical order in cell: hole#, front, side, depth (varies)
            green_depth = pin_front = pin_side_yds = None
            if len(nums) >= 3:
                # depth usually largest reasonable green depth 18-50
                depth_candidates = [n for n in nums if 18 <= n <= 50]
                if depth_candidates:
                    green_depth = max(depth_candidates)
                others = [n for n in nums if n != green_depth]
                if len(others) >= 2:
                    pin_front, pin_side_yds = others[0], others[1]
                elif len(others) == 1:
                    pin_front = others[0]
            elif len(nums) == 2:
                pin_front, green_depth = nums[0], nums[1]
            elif len(nums) == 1:
                green_depth = nums[0]

            if green_depth is None and not nums:
                continue

            holes.append({
                "hole": hole_num,
                "green_depth_yds": green_depth,
                "pin_from_front_yds": pin_front,
                "pin_from_side_yds": pin_side_yds,
                "pin_side": pin_side,
                "near_hazard": False,
            })

    return holes


def main():
    args = parse_args()
    if not EXTRACTED.exists():
        print(f"[ocr-import] Missing extracted images: {EXTRACTED}", file=sys.stderr)
        sys.exit(1)

    images = sorted(
        [p for p in EXTRACTED.iterdir() if p.suffix.lower() in (".png", ".jpg", ".jpeg")],
        key=lambda p: p.name,
    )
    print(f"[ocr-import] Found {len(images)} images in {EXTRACTED}")
    print("[ocr-import] Loading EasyOCR (first run downloads models)…")
    reader = easyocr.Reader(["en"], gpu=False, verbose=False)

    batch = []
    ok = fail = 0
    for i, path in enumerate(images):
        if args.limit and i >= args.limit:
            break
        try:
            items = ocr_image(reader, path)
            header = parse_header(texts_flat(items))
            holes = parse_holes_from_items(items, path)
            if not header["play_date"] or not header["round_num"]:
                raise ValueError(f"header incomplete: {header}")
            if len(holes) < 12:
                raise ValueError(f"only {len(holes)} holes parsed")

            batch.append({
                **header,
                "source_image": path.name,
                "_image_src": str(path),
                "holes": holes,
            })
            ok += 1
            print(f"[ocr-import] OK {path.name}: {header['course_name'][:40]} | {header['play_date']} R{header['round_num']} | {len(holes)} holes")
        except Exception as e:
            fail += 1
            print(f"[ocr-import] FAIL {path.name}: {e}", file=sys.stderr)

    out_path = OUT_DIR / "ocr_batch_all.json"
    out_path.write_text(json.dumps(batch, indent=2), encoding="utf-8")
    print(f"[ocr-import] Wrote {len(batch)} sheets -> {out_path} (ok={ok}, fail={fail})")


if __name__ == "__main__":
    main()
