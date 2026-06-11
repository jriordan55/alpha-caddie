# Free disk after pin import — run once from repo root
Remove-Item -Recurse -Force "$PSScriptRoot\..\data\pin_locations_extracted" -ErrorAction SilentlyContinue
Remove-Item -Recurse -Force "$PSScriptRoot\..\data\pin_locations\_import_staging" -ErrorAction SilentlyContinue
Remove-Item -Force "$PSScriptRoot\..\data\pin_locations\batches\ocr_batch_all.json" -ErrorAction SilentlyContinue
Write-Host "Cleaned pin import temp folders (~800MB)."
