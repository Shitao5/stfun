test_that("multiplication works", {
  expect_equal(cut_value(2, 1, c(1, -1)), -1)
  expect_equal(cut_value(2, 1, LETTERS[1:2]), "B")
  expect_equal(cut_value(2, c(1, 3), c(1, 3, 5)), 3)
  expect_equal(cut_value(NA, 1, LETTERS[1:2], if_na = "Z"), "Z")
  expect_equal(cut_value(NA, c(1, 3), c(1, 3, 5), if_na = 10), 10)
})

