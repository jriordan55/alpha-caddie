# Sportsbook logos

PNG favicons used by Round projections / +EV book marks (`logos/<domain_slug>.png`).

Regenerate:

```bash
node --input-type=module -e "/* see download script in repo history or re-fetch Google favicons */"
```

Or from R at repo root: `source("scripts/download_sportsbook_logos.R")` (writes `www/logos/`; copy into this folder).
