# Download sportsbook favicons to www/logos/ so the app can show real icons
# Run once from the project root: source("scripts/download_sportsbook_logos.R")
# Or in R: setwd("C:/Users/student/Documents/golfModel"); source("scripts/download_sportsbook_logos.R")

book_domains <- c(
  "DraftKings" = "draftkings.com", "FanDuel" = "fanduel.com", "BetMGM" = "betmgm.com",
  "Caesars" = "caesars.com", "Bet365" = "bet365.com", "Bovada" = "bovada.lv",
  "PointsBet" = "pointsbet.com", "William Hill" = "williamhill.com", "Betway" = "betway.com",
  "Unibet" = "unibet.com", "SkyBet" = "skybet.com", "Pinnacle" = "pinnacle.com",
  "BetOnline" = "betonline.ag", "DataGolf" = "datagolf.com", "Circa" = "circasports.com",
  "BetCRIS" = "betcris.com"
)

app_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
logos_dir <- file.path(app_dir, "www", "logos")
if (!dir.exists(logos_dir)) dir.create(logos_dir, recursive = TRUE)

for (i in seq_along(book_domains)) {
  domain <- book_domains[i]
  name <- names(book_domains)[i]
  slug <- gsub("[^a-z0-9]+", "_", tolower(domain))
  url <- paste0("https://www.google.com/s2/favicons?domain=", domain, "&sz=64")
  dest <- file.path(logos_dir, paste0(slug, ".png"))
  tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
    message("OK: ", name, " -> ", dest)
  }, error = function(e) message("Skip: ", name, " - ", conditionMessage(e)))
}

message("Logos saved to ", logos_dir, ". Restart the Shiny app to use them.")
