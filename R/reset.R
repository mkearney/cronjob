#' Reset current cronjobs
#'
#' Resets all crontabs
#'
#' @export
cron_reset <- function() {
  UseMethod("cron_reset")
}

#' @export
cron_reset.default <- function() {
  writeLines(cron_list(silent = TRUE), tmp <- tempfile(fileext = ".txt"))
  cat("Current jobs saved as", tmp, "\n")
  cron_system("-r")
  invisible()
}
