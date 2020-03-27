
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

cron_system <- function(...) {
  cron_check_install()
  system2("crontab", args = c(...), stdout = TRUE)
}

#' @export
cron_list.default <- function(silent = FALSE) {
  x <- cron_system("-l")
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
  cat("Printing current cron jobs\n")
  cron_list()
  cron_system("-r")
}

cron_preample <-
"# An example by {cronjobs}:
#  ┏╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍ minute  (0-59)
#  ╏   ┏╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍ hour    (0-23)
#  ╏   ╏   ┏╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍ day     (1-31)
#  ╏   ╏   ╏   ┏╍╍╍╍╍╍╍╍╍╍╍ month   (1-12;jan-dec)
#  ╏   ╏   ╏   ╏   ┏╍╍╍╍╍╍╍ weekday (0-6;sun-sat)
#  ╏   ╏   ╏   ╏   ╏   ┏╍╍╍ command (+arguments)
#  ╏   ╏   ╏   ╏   ╏   ╏
# [0] [9] [*] [*] [*] [Rscript /home/user/path/to/job.R]
#"

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
  writeLines(c(cron_preample, jobs), tmp <- tempfile())
  cron_system(tmp)
  cron_list()
}

cron_check_install <- function() {
  if (Sys.which("crontab") != "") {
    return(invisible(TRUE))
  }
  if (.Platform$OS.type != "unix") {
    stop("Sorry, crontab is not available for Windows\n", call. = FALSE)
  }
  if (grepl("linux", R.version$platform)) {
    stop("crontab not found. Install using terminal commands such as\n  $ ",
      "sudo apt-get update\n  $ sudo apt-get install cron", call. = FALSE)
  }
  stop("couldn't find crontab on your computer.", call. = FALSE)
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
  if (length(i) == 0) {
    dots <- list(...)
    if (length(dots) > 0) {
      dots <- paste0("jobs, ", paste(names(dots), "=", dots, collapse = ", "))
    } else {
      dots <- "jobs"
    }
    stop("The following returned no matches: grep(\"", x, "\", ", dots, ")",
      call. = FALSE)
  }
  writeLines(jobs[-i], tmp <- tempfile())
  cron_system(tmp)
  cron_list()
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

