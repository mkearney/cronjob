
#' Remove cronjob
#'
#' Removes job to list of crontabs
#'
#' @param x Job to remove–either the full job, the line number, or a regex
#' @param ... other args passed to grep()
#' @export
cron_remove <- function(x, ...) {
  UseMethod("cron_remove")
}

#' @export
cron_remove.default <- function(x, ...) {
  if (length(x) == 0) {
    return(invisible())
  }
  stopifnot(
    is.atomic(x)
  )
  dapr::lap(x, cron_remove_one, ...)
  invisible()
}

cron_remove_one <- function(x, ...) {
  jobs <- cron_list(silent = TRUE)
  if (is.numeric(x)) {
    for (j in seq_along(jobs)) {
      i <- j - 1L
      if (!grepl("^#", jobs[j])) {
        break
      }
    }
    i <- x + i
  } else if (grepl(paste0("^(\\d|\\*){1,2} (\\d|\\*){1,2} (\\d|\\*){1,2} ",
    "(\\d|\\*){1,2} (\\d|\\*){1,2} \\S+"), x)) {
    i <- which(cron_strip(jobs) == cron_strip(x))
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
    warning("The following returned no matches: grep(\"", x, "\", ", dots, ")",
      call. = FALSE)
    return(invisible())
  }
  writeLines(jobs[-i], tmp <- tempfile())
  cron_system(tmp)
  invisible()
}

