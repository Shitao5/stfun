% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_tag.R
\name{clip_insert_spaces}
\alias{clip_insert_spaces}
\title{Insert spaces between numbers and Chinese}
\usage{
clip_insert_spaces()
}
\value{
a piece of text to you Clipboard.
}
\description{
Insert spaces between numbers and Chinese
}
\examples{
\dontrun{
Sys.setenv(CLIPR_ALLOW=TRUE)
clipr::write_clip("hello1!", allow_non_interactive = TRUE)
clip_insert_spaces()
clipr::read_clip()
}
}
