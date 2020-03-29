#' Add cronjob
#'
#' Adds job to list of crontabs
#'
#' @param x Command or path to .R file
#' @param time When to execute
#' @export
cron_add <- function(x, time = NULL) {
  UseMethod("cron_add")
}

#' @export
cron_add.default <- function(x, time = NULL) {
  stopifnot(
    is.character(x),
    is.atomic(time)
  )
  for (i in seq_along(x)) {
    cron_add_one(x[i], time = time)
  }
  invisible()
}

cron_add_one <- function(x, time = NULL) {
  if (!is.null(time)) {
    time <- paste0(trimws(time), " ")
  } else if (!grepl("^(\\d+|\\*) ", x)) {
    time <- "00 * * * * "
  } else {
    time <- ""
  }
  if (grepl("^\\S+\\.R", x)) {
    if (!grepl("/", x)) {
      x <- file.path(getwd(), x)
    }
    x <- paste("Rscript", normalizePath(x, mustWork = FALSE))
  }
  jobs <- trimws(c(cron_list(silent = TRUE), paste0(time, x)))
  jobs <- grep("^#|^$", jobs, invert = TRUE, value = TRUE)
  writeLines(c(cron_preamble, jobs), tmp <- tempfile())
  cron_system(tmp)
  invisible()
}
