#' Extract Highlighted Sentences from a Markdown File
#'
#' This function reads a markdown file, extracts the highlighted sentences (enclosed in **),
#' and writes them to another markdown file.
#'
#' @param input_file Path to the input markdown file.
#' @param output_file Path to the output markdown file.
#'
#' @importFrom dplyr as_tibble filter mutate pull %>%
#' @importFrom stringr str_detect str_extract
#'
#' @export
extract_highlights <- function(input_file, output_file) {
  lines <- readLines(input_file)

  highlighted_sentences <- dplyr::tibble(value = lines) %>%
    dplyr::filter(str_detect(.data$value, "\\*\\*")) %>%
    dplyr::mutate(value = stringr::str_extract(.data$value, "(?<=\\*\\*).*(?=\\*\\*)")) %>%
    dplyr::pull(.data$value)

  writeLines(highlighted_sentences, output_file, sep = "\n\n")

  invisible(NULL)
}
