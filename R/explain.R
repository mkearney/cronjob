
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
  order_input <- c(
    "| Order | Unit         |      | Input | Meaning         |",
    "|–––––––|––––––––––––––|      |–––––––|–––––––––––––––––|",
    "|   1   | minute       |      |   *   | any value       |",
    "|   2   | hour         |      |   ,   | value separator |",
    "|   3   | day (month)  |      |   -   | value range     |",
    "|   4   | month        |      |   /   | step values     |",
    "|   5   | day (week)   |      "
  )
  cat(gray(sub("\n#$", "", cron_preamble)), "\n\n")
  cat(paste(order_input, collapse = "\n"), "\n\n")
  invisible()
}
