#' Calculate the value of x based on the cut points and corresponding segment values
#'
#' @param x A numeric value.
#' @param breaks A numeric vector indicating the cut points, left open and right closed.
#' @param values A vector indicating the return values of each segment, its length must be equal to `breaks` + 1.
#' @param right Logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param if_na A value of the same type as `values` used to determine the value returned when x is NA.
#'
#' @return A value from `values`.
#' @export
#'
#' @examples
#' cut_value(2, 1, c(1, -1))
#' cut_value(2, 1, LETTERS[1:2])
#' cut_value(2, c(1, 3), c(1, 3, 5))
cut_value <- function(x, breaks, values, right = TRUE, if_na = NA) {

  if (length(values) != length(breaks) + 1) {
    stop("The length of `values` must be equal to the length of `breaks` + 1.")
  }

  if (is.na(x)) {
    return(if_na)
  } else {
    breaks <- c(-Inf, breaks, Inf)
    index <- cut(x, breaks = breaks, labels = FALSE, right = right)

    values[[index]]
  }
}
