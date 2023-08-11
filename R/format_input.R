#' Format Text for Data Structures Input
#'
#' This function reads the content of a text file from a file path or clipboard,
#' processes the content, and formats it into a character vector suitable for
#' use as an input to various data structures creation functions.
#'
#' @param con Character string. The connection to the file or "clipboard" to read from the clipboard.
#' @param type Character string. The type of processing to be applied ("text" or "other").
#' @param sep Character string. The separator to be used when concatenating lines.
#'
#' @return A character vector containing the processed and formatted content.
#'
#' @examples
#' \dontrun{
#' # Read from the clipboard and format as text
#' formatted_text <- format_input()
#'
#' # Read from a file path and format as "other"
#' formatted_other <- format_input("path/to/your/file.txt", "other")
#' }
#'
#' @export
format_input <- function(con = "clipboard", type = "text", sep = "\n") {
  text_file_content <- readLines(con)

  processed_content <- switch(type,
                              text = paste0('"', text_file_content[nchar(text_file_content) > 0], '",'),
                              other = paste0(text_file_content[nchar(text_file_content) > 0], ",")
  )

  processed_content[length(processed_content)] <-
    gsub(",$", "", processed_content[length(processed_content)])

  cat(processed_content, sep = sep)
}
