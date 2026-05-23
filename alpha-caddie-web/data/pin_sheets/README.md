# Pin sheet adjustments

Round projections (total score, birdies, pars, bogeys, GIR, fairways) can be nudged from a **ShotLink pin sheet** for the active round.

## Quick workflow (screenshot in chat)

1. Send the pin sheet screenshot in Cursor and ask to update `pin_sheet_active.json` (or save the image here).
2. Optional: save the image as `pin_sheet.png` in this folder.
3. Run `npm run push:live` from the repo (or `npm run apply:pin-sheet` in `alpha-caddie-web`).

`push:live` runs pin adjustments **after weather bake**, before the vs-actual CSV export.

## Files

| File | Purpose |
|------|---------|
| `pin_sheet_active.json` | Active pin positions (set `round` to match `display_round`) |
| `the-cj-cup-byron-nelson-r3.json` | Optional per-event file (auto-matched by name + round) |
| `pin_sheet.png` | Optional; with `OPENAI_API_KEY` + `GOLF_PIN_SHEET_VISION=1` can auto-build JSON |

## JSON shape

```json
{
  "event_name": "THE CJ CUP Byron Nelson",
  "round": 3,
  "source": "screenshot",
  "holes": [
    { "hole": 1, "green_depth_yds": 37, "pin_from_front_yds": 8, "pin_from_side_yds": 5, "pin_side": "L", "near_hazard": false }
  ]
}
```

## Skip

`GOLF_SKIP_PIN_SHEET=1` on `push:live`
