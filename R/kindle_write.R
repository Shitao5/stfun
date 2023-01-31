#' Export to xlsx
#'
#' @param data The data read from [kindle_read()].
#' @param file The file name to write to.
#' @param distinct Logical. If `TRUE`, the last record with the same `begin`
#'  value will be retained; if `FALSE`, no records will be deleted.
#' @param title String. Provide the `title` of the `data` you want to write out,
#'  with support for regular expressions. If `NULL`, all of records will be wrote out.
#' @param columns A character vector. Select and reorder variables in a data frame. Choose from the following values:
#'
#'  - `id`: The row number.
#'  - `title`: Name of the book.
#'  - `page`: The number of page of the excerpt in the book.
#'  - `begin`: The starting position of the excerpt in the book.
#'  - `end`: The ending position of the excerpt in the book.
#'  - `datetime`: The time when the excerpt is took down.
#'  - `text`: Excerpted content.
#'
#' @param drop_na Logical. If `TRUE`, drop rows where `text` column is `NA`; if `FALSE`,
#'  no records will be deleted.
#' @param new_line Logical. If `TRUE`, ` ` will be replaced by `\n` which can create
#' a new line.
#'
#' @return A excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' text <- kindle_read("tests/testthat/My-Clippings.txt")
#' kindle_write_xlsx(text, "tests/testthat/kindle_write.xlsx")
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
kindle_write_xlsx <- function(data, file, columns = NULL,
                         distinct = TRUE,
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
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = dplyr::row_number())
  }

  if (new_line == TRUE) {
    data <- data %>%
      dplyr::mutate(text = stringr::str_replace_all(.data$text, " ", "\n"))
  }

  if (!is.null(columns)) {
    data <- data %>%
      dplyr::select(dplyr::all_of(columns))
  }

  writexl::write_xlsx(data, path = file)

}

#' Export to Markdown
#'
#' @description Write out partial data to a single Markdown file.
#'
#' @param data The data read from [kindle_read()].
#' @param file The file name to write to, book title is recommended.
#' @param title String. Provide the `title` of the `data` you want to write out,
#'  with support for regular expressions.
#' @param distinct Logical. If `TRUE`, the last record with the same `begin`
#'  value will be retained; if `FALSE`, no records will be deleted.
#' @param drop_na Logical. If `TRUE`, drop rows where `text` column is `NA`; if `FALSE`,
#'  no records will be deleted.
#' @param new_line Logical. If `TRUE`, ` ` will be replaced by `\n` which can create
#' a new line.
#' @param time Logical. If `TRUE`, the `datetime` will be add to the end of `text`.
#'
#' @return A Markdown file.
#' @export
#'
#' @examples
#' \dontrun{
#' kindle_write_md(text, "mdtest.md", title = "book-title")
#' }
kindle_write_md <- function(data, file, title = NULL,
                              distinct = TRUE,
                              drop_na = TRUE,
                              new_line = TRUE,
                              time = TRUE) {

  if (!is.null(title)) {
    data <- data %>%
      dplyr::filter(stringr::str_detect(.data$title, {{ title }}))
  } else {
    warning("It is recommended that you add the `title` parameter,
            otherwise all notes will be mixed together and difficult to distinguish.")
  }

  if (drop_na == TRUE) {
    data <- data %>%
      tidyr::drop_na(.data$text)
  }

  if (distinct == TRUE) {
    data <- data %>%
      dplyr::group_by(.data$title, .data$begin) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = dplyr::row_number())
  }

  if (new_line == TRUE) {
    data <- data %>%
      dplyr::mutate(text = stringr::str_replace_all(.data$text, " ", "\n"))
  }

  if (time == TRUE) {
    data <- data %>%
      dplyr::mutate(text = stringr::str_c(.data$text, " \\uff08", .data$datetime, "\\uff09"))
  }

  readr::write_lines(stringi::stri_unescape_unicode(data$text),
                     file = file, sep = "\n\n")
}

