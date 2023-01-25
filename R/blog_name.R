#' Code a name
#'
#' @param name a character
#' @param gap code gap
#'
#' @return a character
#' @export
#'
#' @examples
#' blog_name("gdkkn")
#' blog_name("eg", gap = 2)
#'
blog_name <- function(name, gap = 1) {

  # split `name` into `singles`
  singles <- strsplit(name, split = "") |>
    unlist() |> toupper()

  # get indexes from singles
  index <- vector(mode = "integer", length = length(singles))
  for (i in 1:length(singles)) {
    index[i] <- which(LETTERS == singles[i])
  }

  # generate new indexes
  new_index <- ifelse((index + gap) %% 26 == 0,
                      26, (index + gap) %% 26)

  # get result
  paste0(LETTERS[new_index], collapse = "")
}

