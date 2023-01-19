#' Remove links and tags
#'
#' @description
#' Read from the Clipboard, remove all `[[`, `]]`
#' and `#tag`, which is widely used in note taking tools like
#' *Roam Research* and write to Clipboard again.
#'
#' @usage remove_tag()
#' @return a piece of text to you Clipboard.
#' @export
#'
#' @examples
#' Sys.setenv(CLIPR_ALLOW=TRUE)
#' clipr::write_clip("A [[nice]] day. #happy", allow_non_interactive = TRUE)
#' remove_tag()
#' clipr::read_clip()

remove_tag <- function() {
  clipr::read_clip() |>
    stringr::str_remove_all("\\[\\[") |>
    stringr::str_remove_all("\\]\\]") |>
    stringr::str_remove_all(" #.*") |>
    writeLines(con = "clipboard", sep = "")
}
