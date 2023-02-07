#' The answers of your life
#'
#' @param n The number of answers returned.
#'
#' @return Some text
#' @export
#'
#' @examples
#' \dontrun{
#' life_answers()
#' life_answers(n = 2)
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
life_answers <- function(n = 1) {
  text <- answers %>%
    dplyr::slice_sample(n = n, replace = TRUE) %>%
    dplyr::pull() %>%
    stringi::stri_unescape_unicode()

  paste(text, sep = "\n")
}
