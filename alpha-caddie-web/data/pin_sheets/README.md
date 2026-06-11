# Pin sheet adjustments (projections)

Round projections can be nudged from a **ShotLink pin sheet** for the **upcoming display round** — only when you send the tee sheet.

## Workflow

1. Send the current-round pin sheet screenshot in chat (or save as `data/pin_sheets/pin_sheet.png`).
2. We update `pin_sheet_active.json` with hole data, matching `round` / `event_name`, and **`"apply_to_projections": true`**.
3. Run **`npm run push:live`** (or `npm run apply:pin-sheet`).

On push, when the sheet is armed:

- Projections get pin-setup nudges (birdies, bogeys, total, GIR, fairways).
- The sheet is **saved to `data/pin_locations/`** (course + play date + round) for Historical Trends.
- `pin_sheet.png` is copied into `data/pin_locations/images/` when present.

**Nothing changes** if `apply_to_projections` is not `true`, or if event/round doesn’t match `display_round`.

```json
{
  "apply_to_projections": true,
  "event_name": "Memorial Tournament presented by Workday",
  "course_name": "Muirfield Village Golf Club",
  "round": 2,
  "play_date": "2026-06-05",
  "source": "screenshot",
  "holes": [ { "hole": 1, "green_depth_yds": 35, "pin_from_front_yds": 14, "pin_from_side_yds": 5, "pin_side": "R" } ]
}
```

`play_date` is optional — derived from projections field start / event dates when omitted.

Set `"apply_to_projections": false` when the round is over or you don’t want pin nudges.

## Screenshot auto-parse

If `pin_sheet.png` is **newer** than `pin_sheet_active.json` and `OPENAI_API_KEY` is set, `push:live` / `apply:pin-sheet` vision-parses the image first (sets `apply_to_projections: true`). Or set `GOLF_PIN_SHEET_VISION=1` to force parse when the PNG is older.

## Historical pin database

The archive in `data/pin_locations/` powers **Historical Trends** and hole-by-hole history. Past sheets from the zip import live there; **new** sheets you send are appended on each armed `push:live`.

See [`data/pin_locations/README.md`](../../../data/pin_locations/README.md).

## Files

| File | Purpose |
|------|---------|
| `pin_sheet_active.json` | Current tee sheet; set `apply_to_projections: true` to apply on push |
| `pin_sheet.png` | Screenshot you send; auto-parsed when newer than JSON |

## Skip

`GOLF_SKIP_PIN_SHEET=1` on `push:live`
