#' Calculate the percentile of value x in a set of numbers
#'
#' @param x A numeric value.
#' @param vector A set of numeric values.
#' @param digits Integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @param na.rm Logical. Should missing values (including NaN) be removed?

#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' percentile(2, c(1, 5))
#' percentile(2, c(1, 3, 5), digits = 2)
percentile <- function(x, vector, digits = 4, na.rm = TRUE) {
  round(sum(vector < x, na.rm = na.rm) / length(vector), digits = digits)
}
