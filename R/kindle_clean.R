#' Clean Kindle file
#'
#' @param data The data read from [kindle_read()].
#' @param distinct Logical. If `TRUE`, the last record with the same `begin`
#'  value will be retained; if `FALSE`, no records will be deleted.
#' @param title String. Provide the `title` of the `data` you want to write out,
#'  with support for regular expressions. If `NULL`, all of records will be wrote out.
#' @param drop_na Logical. If `TRUE`, drop rows where `text` column is `NA`; if `FALSE`,
#'  no records will be deleted.
#' @param new_line Logical. If `TRUE`, ` ` will be replaced by `\n` which can create
#' a new line.
#'
#' @return A tidy data.
#' @export
#'
#' @examples
#' \dontrun{
#' text <- kindle_read("tests/testthat/My-Clippings.txt")
#' kindle_clean(text)
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
kindle_clean <- function(data, distinct = TRUE,
                              drop_na = TRUE,
                              new_line = TRUE,
                              title = NULL) {

  if (drop_na == TRUE) {
    data <- data %>%
      tidyr::drop_na(.data$text)
  }

  if (!is.null(title)) {
    data <- data %>%
      dplyr::filter(stringr::str_detect(.data$title, {{ title }}))
  }

  if (distinct == TRUE) {
    data <- data %>%
      dplyr::group_by(.data$title, .data$begin) %>%
      dplyr::arrange(.data$begin, .data$datetime, .by_group = TRUE) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = dplyr::row_number())
  } else {
    data <- data %>%
      dplyr::group_by(.data$title, .data$begin) %>%
      dplyr::arrange(.data$begin, .data$datetime, .by_group = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = dplyr::row_number())
  }

  if (new_line == TRUE) {
    data <- data %>%
      dplyr::mutate(text = stringr::str_replace_all(.data$text, " ", "\n"))
  }

  data
}

