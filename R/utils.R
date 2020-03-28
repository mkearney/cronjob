gray <- function(x) {
  paste0("\033[38;5;246m", x, "\033[39m")
}

cron_system <- function(...) {
  cron_check_install()
  system2("crontab", args = c(...), stdout = TRUE)
}

cron_preamble <-
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

cron_strip <- function(x) gsub("\\b0(?=\\d)|\\s", "", x, perl = TRUE)
