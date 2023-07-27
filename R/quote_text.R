#' Read and Process Text File Content
#'
#' This function reads the content of a text file from the clipboard or a file path,
#' processes the content, and returns the processed text as a character vector.
#'
#' @param con Character string. The connection to the file or "clipboard" to read from the clipboard.
#'
#' @return A character vector containing the processed content of the text file.
#'
#' @examples
#' \dontrun{
#' # Read from the clipboard
#' processed_content <- quote_text()
#'
#' # Read from a file path
#' processed_content <- quote_text("path/to/your/file.txt")
#' }
#' @export
quote_text <- function(con = "clipboard") {
  text_file_content <- readLines(con)
  processed_content <- paste0('"', text_file_content[nchar(text_file_content) > 0], '",')
  processed_content[length(processed_content)] <- gsub(',$', '', processed_content[length(processed_content)])

  cat(processed_content, sep = "\n")
}
