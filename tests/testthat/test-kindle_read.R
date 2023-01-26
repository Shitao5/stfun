test_that("test kindle_read()", {
  test_data <- kindle_read(test_path("My-Clippings.txt"))
  result <- structure(list(id = 1:2, title = c("Eloquent JavaScript (Marijn Haverbeke)",
                                               "Eloquent JavaScript (Marijn Haverbeke)"), page = c("78", "82"
                                               ), begin = c("1195", "1257"), end = c("1195", "1258"), datetime = structure(c(1667177610,
                                                                                                                             1667259831), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                           text = c("The standard JavaScript functions, and most JavaScript programmers, follow the bottom style—they capitalize every word except the first.",
                                    "Creating new words in prose。")), row.names = c(NA, -2L
                                    ), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(test_data, result)
})
