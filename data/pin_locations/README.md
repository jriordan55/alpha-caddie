# Pin locations database

ShotLink pin location sheets keyed by **course + play date + round** (not tournament name). Each grid square on the official sheets is **5 yards** per side.

## Layout

```
data/pin_locations/
  index.json              # lookup table: course_key|YYYY-MM-DD|round → sheet path
  sheets/{course_key}/    # one JSON per course×date×round
    2025-06-05_r1.json
  images/                 # source PNGs from pin_locations.zip (optional)
```

## Import from zip

Source archive: `data/pin_locations.zip` (243 PNG pin sheets). After import, PNGs live under `images/`; the zip may be removed to save disk space.

```bash
cd alpha-caddie-web
set OPENAI_API_KEY=sk-...   # required for vision parse
npm run import:pin-locations
npm run sync:pin-locations
```

Options:

- `npm run import:pin-locations -- --limit 5` — test on first 5 images
- `npm run import:pin-locations -- --force` — re-parse even if already indexed
- `PIN_LOCATIONS_ZIP` — override zip path
- `PIN_LOCATIONS_DIR` — override DB root (default: `data/pin_locations`)

## Integration

| Consumer | Lookup key |
|----------|------------|
| **Live projections** | Manual only — `pin_sheet_active.json` with `apply_to_projections: true`; saved here on `push:live` |
| `build-player-history` | `course_name` + derived play date + `round_num` |
| Hole-by-hole history | `tournament_id` (event_id) + round → pin sheet |

When you send an upcoming-round tee sheet and run `push:live`, `apply:pin-sheet` writes the armed sheet into this DB, then `sync:pin-locations` mirrors it for deploy.

## Sheet JSON shape

```json
{
  "course_key": "tpc toronto at osprey valley",
  "course_name": "TPC Toronto at Osprey Valley (North Course)",
  "play_date": "2025-06-05",
  "round_num": 1,
  "event_name_ref": "RBC Canadian Open",
  "grid_yards_per_square": 5,
  "holes": [
    {
      "hole": 1,
      "green_depth_yds": 32,
      "pin_from_front_yds": 6,
      "pin_from_side_yds": 4,
      "pin_side": "L",
      "pin_grid_from_front": 1.2,
      "pin_grid_from_side": 0.8
    }
  ]
}
```
