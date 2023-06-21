#' Learning Progress
#'
#' Calculate and display the learning progress.
#'
#' @param now The current progress of learning.
#' @param total The total amount of learning materials.
#'
#' @return A string indicating the learning progress.
#' @examples
#' progress(50, 100)
#'
#' @export
progress <- function(now, total) {
  paste0(
    "Learning Progress: ",
    round(now / total * 100, 2),
    "%."
  )
}
