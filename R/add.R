#' Add cronjob
#'
#' Adds job to list of crontabs
#'
#' @param cmd Command or path to .R file
#' @param time When to execute
#' @export
cron_add <- function(cmd, time = NULL) {
  UseMethod("cron_add")
}

#' @export
cron_add.default <- function(cmd, time = NULL) {
  stopifnot(
    is.character(x),
    is.atomic(time)
  )
  for (i in seq_along(cmd)) {
    cron_add_one(cmd[i], time = time)
  }
  invisible()
}

cron_add_one <- function(cmd, time = NULL) {
  if (!is.null(time)) {
    time <- paste0(trimws(time), " ")
  } else if (!grepl("^(\\d+|\\*) ", cmd)) {
    time <- "00 * * * * "
  } else {
    time <- ""
  }
  if (grepl("^\\S+\\.R", cmd)) {
    if (!grepl("/", cmd)) {
      cmd <- file.path(getwd(), cmd)
    }
    cmd <- paste("Rscript", normalizePath(cmd, mustWork = FALSE))
  }
  jobs <- trimws(c(cron_list(silent = TRUE), paste0(time, cmd)))
  jobs <- grep("^#|^$", jobs, invert = TRUE, value = TRUE)
  writeLines(c(cron_preamble, jobs), tmp <- tempfile())
  cron_system(tmp)
  invisible()
}
