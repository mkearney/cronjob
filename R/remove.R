
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
  if (length(x) == 0) {
    return(invisible())
  }
  stopifnot(
    is.character(x),
    is.logical(exact)
  )
  dapr::lap(x, cron_remove_one, exact = exact, ...)
  invisible()
}

cron_remove_one <- function(x, exact = FALSE, ...) {
  jobs <- cron_list(silent = TRUE)
  if (exact) {
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

