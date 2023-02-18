#' Remove links and tags
#'
#' @description
#' Read from the Clipboard, remove all `[[`, `]]`
#' and `#tag`, which is widely used in note taking tools like
#' *Roam Research* and write to Clipboard again.
#'
#' @return a piece of text to your Clipboard.
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(CLIPR_ALLOW=TRUE)
#' clipr::write_clip("A [[nice]] day. #happy", allow_non_interactive = TRUE)
#' clip_remove_tag()
#' clipr::read_clip()
#' }
clip_remove_tag <- function() {
  clipr::read_clip() |>
    stringr::str_remove_all("\\[\\[") |>
    stringr::str_remove_all("\\]\\]") |>
    stringr::str_remove_all(" #.*") |>
    writeLines(con = "clipboard", sep = "")
}

#' Insert spaces between numbers and Chinese
#'
#' @return a piece of text to you Clipboard.
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(CLIPR_ALLOW=TRUE)
#' clipr::write_clip("hello1!", allow_non_interactive = TRUE)
#' clip_insert_spaces()
#' clipr::read_clip()
#' }
clip_insert_spaces <- function() {
  clipr::read_clip() |>
    stringr::str_replace_all(" *(\\d+) *", " \\1 ") |>
    stringr::str_replace_all("(\\d+) ([\\p{P}])", "\\1\\2") |>
    stringr::str_replace_all("([\\p{P}]) (\\d+)", "\\1\\2") |>
    stringr::str_replace_all("(\\d+\\%)", "\\1 ") |>
    stringr::str_remove_all("^ +") |>
    writeLines(con = "clipboard", sep = "")
}


#' Converts Chinese inverted commas to right angle inverted commas
#'
#' @param reverse Logical. The default is `FALSE`, which converts inverted commas to right-angle inverted commas; if `TRUE`, it converts right-angle inverted commas to inverted commas.
#'
#' @return a piece of text to your Clipboard.
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(CLIPR_ALLOW=TRUE)
#' clipr::write_clip("Sentences-containing-Chinese-inverted-commas.", allow_non_interactive = TRUE)
#' clip_convert_commas()
#' clipr::read_clip()
#' }
clip_convert_commas <- function(reverse = FALSE) {
  if (reverse == FALSE) {
    clipr::read_clip() |>
      stringr::str_replace_all("\\u201c", "\\u300c") |>
      stringr::str_replace_all("\\u201d", "\\u300d") |>
      writeLines(con = "clipboard", sep = "")
  } else {
    clipr::read_clip() |>
      stringr::str_replace_all("\\u300c", "\\u201c") |>
      stringr::str_replace_all("\\u300d", "\\u201d") |>
      writeLines(con = "clipboard", sep = "")
  }
}
