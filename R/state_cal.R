#' Calculate Financial Statement Type
#'
#' @param begin_date A string representing the start date of the financial statement period. The format should be in 'YYYY-MM-DD'.
#' @param end_date A string representing the end date of the financial statement period. The format should be in 'YYYY-MM-DD'.
#'
#' @return A string which contains the type of the financial statement.
#' @export
#'
#' @examples state_type("2023-01-01", "2023-12-31")
state_type <- function(begin_date, end_date) {
  diff <- lubridate::month(end_date) - lubridate::month(begin_date)
  if (diff == 0) {
    return("momthly")
  } else if (diff == 2) {
    return("quarterly")
  } else if (diff == 11) {
    return("yearly")
  }
}

#' Calculate Financial Statement Date Range and Type
#'
#' @param begin_date A string representing the start date of the financial statement period. The format should be in 'YYYY-MM-DD'.
#' @param end_date A string representing the end date of the financial statement period. The format should be in 'YYYY-MM-DD'.
#'
#' @return A string which contains the date range and type of the financial statement.
#' @export
#'
#' @examples
#' state_info("2023-01-01", "2023-12-31")
state_info <- function(begin_date, end_date) {
  diff <- lubridate::month(end_date) - lubridate::month(begin_date)
  year <- lubridate::year(end_date)
  month <- lubridate::month(end_date)
  if (diff == 0) {
    paste0(year, "M", month)
  } else if (diff == 2) {
    paste0(year, "Q", month / 3)
  } else if (diff == 11) {
    paste0(year, "year")
  }
}








