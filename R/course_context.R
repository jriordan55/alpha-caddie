# Shared course resolution for shot-level model + round_projections alignment.
# SELECTED_COURSE is written to golf_selected_course.txt by round_projections.R when the pipeline runs.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Same normalization as round_projections.R / app.R
normalize_course_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  n <- length(x)
  if (n == 0) return(character(0))
  out <- rep("", n)
  mask_pga <- grepl("pga national", x, fixed = TRUE) & grepl("champion", x, fixed = TRUE)
  out[mask_pga] <- "pga national champion"
  other <- !mask_pga & nzchar(x)
  if (any(other)) {
    y <- x[other]
    y <- gsub("resort", "", y, fixed = TRUE)
    y <- gsub(" course", "", y, fixed = TRUE)
    y <- gsub(" the ", " ", y, fixed = TRUE)
    y <- gsub("\\s+", " ", y)
    out[other] <- trimws(y)
  }
  out
}

#' Resolve course name: env SELECTED_COURSE > golf_selected_course.txt > course_table first row.
read_selected_course <- function(model_dir = getwd()) {
  e <- trimws(Sys.getenv("SELECTED_COURSE", ""))
  if (nzchar(e)) return(e)
  p <- file.path(model_dir, "golf_selected_course.txt")
  if (file.exists(p)) {
    x <- trimws(readLines(p, warn = FALSE)[1])
    if (nzchar(x)) return(x)
  }
  ct <- file.path(model_dir, "data", "course_table.csv")
  if (file.exists(ct)) {
    tab <- utils::read.csv(ct, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(tab) > 0 && "course" %in% names(tab)) {
      x <- trimws(as.character(tab$course[1]))
      if (nzchar(x)) return(x)
    }
  }
  NA_character_
}

course_row_matches_selected <- function(schedule_course_name, selected_course) {
  if (is.na(selected_course) || !nzchar(as.character(selected_course))) return(FALSE)
  a <- normalize_course_name(schedule_course_name)
  b <- normalize_course_name(selected_course)
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (a == b) return(TRUE)
  if (grepl(b, a, fixed = TRUE) || grepl(a, b, fixed = TRUE)) return(TRUE)
  FALSE
}

#' Stack pgatouR schedules — tournament_id -> course_name (as played that week).
build_schedule_course_map <- function(years = 2021:2026) {
  if (!requireNamespace("pgatouR", quietly = TRUE)) stop("Install pgatouR.")
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[[i]]
    s <- pgatouR::pga_schedule(y)
    out[[i]] <- data.frame(
      tournament_id = as.character(s$tournament_id),
      course_name = as.character(s$course_name),
      tournament_name = as.character(s$tournament_name),
      year = y,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

#' Keep only shot rows whose tournament maps to SELECTED_COURSE via schedule course_name.
filter_shots_for_selected_course <- function(shots, selected_course, schedule_map) {
  if (is.na(selected_course) || !nzchar(as.character(selected_course))) {
    message("No SELECTED_COURSE — using all shots (tour-wide).")
    return(shots)
  }
  keep_sched <- vapply(seq_len(nrow(schedule_map)), function(i) {
    course_row_matches_selected(schedule_map$course_name[i], selected_course)
  }, logical(1))
  tids <- unique(schedule_map$tournament_id[keep_sched])
  if (length(tids) == 0L) {
    warning("No tournaments in schedule match SELECTED_COURSE=\"", selected_course, "\" — using all shots.")
    return(shots)
  }
  tid <- as.character(shots$tournament_id)
  out <- shots[tid %in% tids, , drop = FALSE]
  message("Course filter \"", selected_course, "\": kept ", nrow(out), " / ", nrow(shots),
          " rows across ", length(tids), " tournaments.")
  out
}

#' Build 18-hole par and yardage vectors from filtered shot data (mean yardage per hole).
course_hole_template_from_shots <- function(shots) {
  if (is.null(shots) || nrow(shots) == 0) return(NULL)
  hn <- suppressWarnings(as.integer(shots$hole_number))
  ok <- is.finite(hn) & hn >= 1L & hn <= 18L
  if (!any(ok)) return(NULL)
  d <- shots[ok, , drop = FALSE]
  d$hole_number <- as.integer(d$hole_number)
  d$par <- suppressWarnings(as.numeric(d$par))
  d$yardage <- suppressWarnings(as.numeric(d$yardage))
  holes_par <- rep(4L, 18L)
  holes_yard <- rep(400, 18L)
  for (h in 1:18) {
    w <- d$hole_number == h
    if (!any(w)) next
    holes_par[h] <- as.integer(round(stats::median(d$par[w], na.rm = TRUE)))
    if (!is.finite(holes_par[h]) || holes_par[h] < 3L) holes_par[h] <- 4L
    ym <- mean(d$yardage[w], na.rm = TRUE)
    if (is.finite(ym) && ym > 50) holes_yard[h] <- as.integer(round(ym))
  }
  list(par = holes_par, yardage = holes_yard)
}

#' Fallback 18-hole template from course_table row (total par + total yardage spread by default shape).
course_hole_template_from_course_table <- function(model_dir, selected_course) {
  path <- file.path(model_dir, "data", "course_table.csv")
  if (!file.exists(path)) return(NULL)
  tab <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(tab) < 1 || !"course" %in% names(tab)) return(NULL)
  sk <- normalize_course_name(selected_course)
  tab$course_key <- normalize_course_name(tab$course)
  row <- tab[tab$course_key == sk, , drop = FALSE]
  if (nrow(row) == 0) row <- tab[1, , drop = FALSE]
  total_par <- suppressWarnings(as.integer(row$par[1]))
  if (!is.finite(total_par) || total_par < 69) total_par <- 72L
  base_pars <- c(rep(3L, 4L), rep(5L, 4L), rep(4L, 10L))
  diff <- total_par - sum(base_pars)
  if (diff != 0) {
    idx <- which(base_pars == 4L)
    base_pars[idx[1:abs(diff)]] <- base_pars[idx[1:abs(diff)]] + as.integer(sign(diff))
  }
  ty <- suppressWarnings(as.numeric(row$yardage[1]))
  if (!is.finite(ty) || ty < 5000) ty <- 7200
  # Spread total yardage by par (rough weights)
  w <- ifelse(base_pars == 3, 0.2, ifelse(base_pars == 5, 0.35, 0.3))
  w <- w / sum(w)
  yard <- as.integer(round(ty * w))
  list(par = base_pars, yardage = yard)
}

#' Build per-hole par and yardage for SELECTED_COURSE from all_shots (schedule filter) or course_table fallback.
#' Called by round_projections.R so Hole Hangout + shot sim use the same layout as the live tournament context.
#'
#' @return list(par, yardage, source) or NULL
refresh_current_course_hole_template <- function(model_dir, selected_course) {
  if (is.na(selected_course) || !nzchar(trimws(as.character(selected_course)))) return(NULL)
  selected_course <- trimws(as.character(selected_course))

  shots_path <- file.path(model_dir, "data", "all_shots_2021_2026.csv")
  sched_path <- file.path(model_dir, "data", "pga_tournament_course_map.csv")
  if (!file.exists(shots_path)) {
    tpl <- course_hole_template_from_course_table(model_dir, selected_course)
    if (is.null(tpl)) return(NULL)
    return(list(par = tpl$par, yardage = tpl$yardage, source = "course_table_only"))
  }

  if (!file.exists(sched_path)) {
    tpl <- course_hole_template_from_course_table(model_dir, selected_course)
    if (is.null(tpl)) return(NULL)
    return(list(par = tpl$par, yardage = tpl$yardage, source = "course_table_no_schedule"))
  }

  schedule_map <- utils::read.csv(sched_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(schedule_map) < 1 || !all(c("tournament_id", "course_name") %in% names(schedule_map))) {
    tpl <- course_hole_template_from_course_table(model_dir, selected_course)
    if (is.null(tpl)) return(NULL)
    return(list(par = tpl$par, yardage = tpl$yardage, source = "course_table_bad_schedule"))
  }

  sh <- NULL
  if (requireNamespace("data.table", quietly = TRUE)) {
    sh <- tryCatch(
      data.table::fread(
        shots_path,
        select = c("hole_number", "par", "yardage", "tournament_id"),
        showProgress = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(sh)) {
      sh <- tryCatch(
        {
          dt <- data.table::fread(shots_path, showProgress = FALSE)
          cn <- intersect(c("hole_number", "par", "yardage", "tournament_id"), names(dt))
          if (length(cn) < 4L) NULL else as.data.frame(dt[, cn, with = FALSE])
        },
        error = function(e) NULL
      )
    }
    if (!is.null(sh)) sh <- as.data.frame(sh)
  }
  if (is.null(sh)) {
    sh <- tryCatch(
      utils::read.csv(shots_path, stringsAsFactors = FALSE, check.names = FALSE)
      , error = function(e) NULL)
    if (!is.null(sh)) {
      keep <- c("hole_number", "par", "yardage", "tournament_id")
      keep <- keep[keep %in% names(sh)]
      if (length(keep) < 4L) sh <- NULL else sh <- sh[, keep, drop = FALSE]
    }
  }
  if (is.null(sh) || nrow(sh) < 1L) {
    tpl <- course_hole_template_from_course_table(model_dir, selected_course)
    if (is.null(tpl)) return(NULL)
    return(list(par = tpl$par, yardage = tpl$yardage, source = "course_table_no_shots"))
  }

  sh_f <- filter_shots_for_selected_course(sh, selected_course, schedule_map)
  tpl <- NULL
  src <- "shots_schedule"
  if (!is.null(sh_f) && nrow(sh_f) >= 100L) {
    tpl <- course_hole_template_from_shots(sh_f)
  }
  if (is.null(tpl) || !is.finite(sum(tpl$par))) {
    tpl <- course_hole_template_from_course_table(model_dir, selected_course)
    if (is.null(tpl)) return(NULL)
    src <- "course_table_fallback"
  }
  list(par = tpl$par, yardage = tpl$yardage, source = src)
}
