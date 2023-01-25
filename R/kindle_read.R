#' Read `My Clippings.txt` from Kindle and clean it into tidy data
#'
#' @param file The path to `My Clippings.txt`, generally in `Kindle/Documents/`
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang .data
#' @importFrom dplyr %>%
kindle_read <- function(file) {
  readLines(file) %>%
  tidyr::as_tibble() %>%
  dplyr::filter(.data$value != "") %>%
  dplyr::add_row(value = "==========", .before = 1) %>%
  dplyr::mutate(group = ifelse(stringr::str_detect(.data$value, "==="),
                               dplyr::row_number(), NA)) %>%
  tidyr::fill(.data$group, .direction = "down") %>%
  dplyr::filter(.data$value != "==========") %>%
  dplyr::group_by(.data$group) %>%
  dplyr::mutate(type = dplyr::case_when(
    dplyr::row_number() == 1 ~ "title",
    dplyr::row_number() == 2 ~ "info",
    dplyr::row_number() == 3 ~ "text",
  )) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = .data$type,
              values_from = .data$value) %>%
  dplyr::mutate(page = stringr::str_extract(.data$info, "(?<=\\u7b2c )\\d+"),
                begin = stringr::str_extract(.data$info, "(?<=#)\\d+"),
                end = stringr::str_extract(.data$info, "(?<=\\-)\\d+"),
                datetime = stringr::str_extract(.data$info, "(?<= )\\d+\\u5e74.*:\\d{2}")
                    %>% lubridate::as_datetime(),
                id = dplyr::row_number()) %>%
  dplyr::select(.data$id, .data$page, .data$begin, .data$end,
                .data$datetime, .data$title, .data$text)
}




