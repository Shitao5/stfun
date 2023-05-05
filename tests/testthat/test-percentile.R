test_that("multiplication works", {
  expect_equal(percentile(2, c(2, 2, 3, 3, 5)), 0)
  expect_equal(percentile(2, c(1, 5)), 0.5)
})
