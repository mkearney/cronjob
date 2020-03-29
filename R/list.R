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
    n <- length(xx[-cmt])
    dig <- nchar(n)
    xx[-cmt] <- sprintf(paste0("\033[38;5;246m[%", dig, "d]\033[39m %s"),
      seq_along(xx[-cmt]), xx[-cmt])
    cat(paste(xx, collapse = "\n"), "\n")
  }
  invisible(x)
}
