
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

gray <- function(x) {
  paste0("\033[38;5;246m", x, "\033[39m")
}

#' @export
cron_list.default <- function(silent = FALSE) {
  x <- system("crontab -l", intern = TRUE)
  if (!silent) {
    xx <- x
    cmt <- grep("^#", xx)
    xx[cmt] <- gray(xx[cmt])
    cat(paste(xx, collapse = "\n"))
  }
  invisible(x)
}

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
  system("crontab -r")
}

cron_preample <- "# created by the R package {cronjobs}

# Example of cron job:
# +-------------- minute (0 - 59)
# | +------------ hour (0 - 23)
# | | +---------- day of month (1 - 31)
# | | | +-------- month (1 - 12) OR jan,feb,mar,apr ...
# | | | | +------ day of week (0 - 6) (Sunday=0 or 7) OR sun,mon,tue,wed,thu,fri,sat
# | | | | |
# * * * * * Rscript /home/user/path/to/job.R"

#' Add cronjob
#'
#' Adds job to list of crontabs
#'
#' @param cmd Command or path to .R file
#' @param time When to execute
#' @export
cron_add <- function(cmd, time = "0 * * * *") {
  UseMethod("cron_add")
}

#' @export
cron_add.default <- function(cmd, time = "00 * * * *") {
  if (grepl("^\\S+\\.R", cmd)) {
    if (!grepl("/", cmd)) {
      cmd <- file.path(getwd(), cmd)
    }
    cmd <- paste("Rscript", normalizePath(cmd, mustWork = FALSE))
  }
  jobs <- trimws(c(cron_list(silent = TRUE), paste(time, cmd)))
  jobs <- grep("^#|^$", jobs, invert = TRUE, value = TRUE)
  writeLines(c(cron_preample, jobs), tmp <- tempfile())
  system(paste("crontab", tmp))
  cron_list()
}

#' Remove cronjob
#'
#' Removes job to list of crontabs
#'
#' @param x Job to remove
#' @param exact Logical indicating whether to due exact matching; the default,
#'   FALSE, will use x as the pattern with grep()
#' @param ... other args passed to grep()
#' @export
cron_remove <- function(x, exact = FALSE, ...) {
  UseMethod("cron_remove")
}

#' @export
cron_remove.default <- function(x, exact = FALSE, ...) {
  jobs <- cron_list(silent = TRUE)
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
cron_explain <- function() {
  UseMethod("cron_explain")
}

#' @export
cron_explain.default <- function() {
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

