#' Export to xlsx
#'
#' @param data The data read from [kindle_read()].
#' @param file A file name to write to.
#' @param distinct Logical. If `TRUE`, the last record with the same `begin`
#'  value will be retained; if `FALSE`, no records will be deleted.
#' @param title String. Provide the `title` of the `data` you want to write out,
#'  with support for regular expressions. If `NULL`, all of records will be writed out.
#' @param columns A character vector. Select and reorder variables in a data frame. Choose from the following values:
#'
#'  - `id`: The row number.
#'  - `title`: Name of the book.
#'  - `page`: The number of page of the excerpt in the book.
#'  - `begin`: The starting position of the excerpt in the book.
#'  - `end`: The end position of the excerpt in the book.
#'  - `datetime`: The time when the excerpt is took down.
#'  - `text`: Excerpted content.
#'
#' @param drop_na Logical. If `TRUE`, drop rows where `text` column is `NA`; if `FALSE`,
#'  no records will be deleted.
#'
#' @return A excel file.
#'
#' @examples
#' text <- kindle_read(test_path("My-Clippings.txt"))
#' kindle_write_xlsx(text, "file.xlsx")
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
kindle_write_xlsx <- function(data, file, columns = NULL,
                         distinct = TRUE,
                         drop_na = TRUE,
                         title = NULL) {

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

  if (!is.null(title)) {
    data <- data %>%
      dplyr::filter(stringr::str_detect(.data$title, {{ title }}))
  }

  if (!is.null(columns)) {
    data <- data %>%
      dplyr::select(dplyr::all_of(columns))
  }

  writexl::write_xlsx(data, path = file)

}
