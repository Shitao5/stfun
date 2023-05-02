test_that("multiplication works", {
  expect_equal(cut_value(2, 1, c(1, -1)), -1)
  expect_equal(cut_value(2, 1, LETTERS[1:2]), "B")
  expect_equal(cut_value(2, c(1, 3), c(1, 3, 5)), 3)
})

