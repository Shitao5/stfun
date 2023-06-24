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

  emoji <- c(
    "\U0001f600", # smile
    "\U0001f973", # party face
    "\U0001f638", # cat grin
    "\U0001f308", # rainbow
    "\U0001f947", # gold medal
    "\U0001f389", # party popper
    "\U0001f38a"  # confetti ball
  )

  if (now >= total) {
    paste0(
      "Learning Progress: ",
      round(now / total * 100, 2),
      "%.",
      sample(emoji, 1)
    )
  } else {
    paste0(
      "Learning Progress: ",
      round(now / total * 100, 2),
      "%."
    )
  }
}


