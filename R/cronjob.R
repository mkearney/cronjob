
#' Get current cronjobs
#'
#' Returns list of crontabs
#'
#' @param silent Logical.
#' @return Current crontabs
#' @export
cronjob_list <- function(silent = FALSE) {
  x <- system("crontab -l", intern = TRUE)
  if (!silent) {
    cat(paste(c(paste0("[", seq_along(x), "] ", x)), collapse = "\n"))
  }
  invisible(x)
}

#' Reset current cronjobs
#'
#' Resets all crontabs
#'
#' @export
cronjob_reset <- function() {
  system("crontab -r")
}

#' Add cronjob
#'
#' Adds job to list of crontabs
#'
#' @param cmd Command or path to .R file
#' @param time When to execute
#' @export
cronjob_add <- function(cmd, time = "0 * * * *") {
  if (grepl("^\\S+\\.R", cmd)) {
    cmd <- paste("Rscript", normalizePath(cmd))
  }
  jobs <- c(cronjob_list(silent = TRUE), paste(time, cmd))
  writeLines(jobs, tmp <- tempfile())
  system(paste("crontab", tmp))
  cronjob_list()
}

#' Remove cronjob
#'
#' Removes job to list of crontabs
#'
#' @param x Job to remove
#' @param exact Logical indicating whether to due exact matching; the default,
#'   FALSE, will use x as the pattern with grep()
#' @param other args passed to grep()
#' @export
cronjob_remove <- function(x, exact = FALSE, ...) {
  jobs <- cronjob_list(silent = TRUE)
  if (exact) {
    i <- which(jobs == x)
  } else {
    i <- grep(x, jobs, ...)
  }
  writeLines(jobs[-i], tmp <- tempfile())
  system(paste("crontab", tmp))
}

#' Explain cronjobs
#'
#' Provides explanation of values and order
#'
#' @return list of data frames about inputs and units
#' @export
cronjob_explain <- function() {
  d1 <- data.frame(
    input = c("*", ",", "-", "/"),
    meaning = c("any value", "value list separator", "range of values", "step values")
  )
  d2 <- data.frame(
    order = seq.int(1, 5),
    unit = c("minute", "hour", "day (month)", "month", "day (week)")
  )
  list(inputs = d1, units = d2)
}

