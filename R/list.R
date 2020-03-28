#' Get current cronjobs
#'
#' Returns list of crontabs
#'
#' @param silent Logical.
#' @return Current crontabs
#' @export
cron_list <- function(silent = FALSE) {
  UseMethod("cron_list")
}

#' @export
cron_list.default <- function(silent = FALSE) {
  x <- cron_system("-l")
  if (!silent) {
    xx <- x
    cmt <- grep("^#", xx)
    xx[cmt] <- gray(xx[cmt])
    cat(paste(xx, collapse = "\n"), "\n")
  }
  invisible(x)
}
