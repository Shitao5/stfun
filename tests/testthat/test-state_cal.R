test_that("multiplication works", {
  expect_equal(state_info("2023-01-01", "2023-12-31"), "2023year")
  expect_equal(state_info("2023-01-01", "2023-03-31"), "2023Q1")
  expect_equal(state_info("2023-01-01", "2023-01-31"), "2023M1")
  expect_equal(state_type("2023-01-01", "2023-12-31"), "yearly")
  expect_equal(state_type("2023-01-01", "2023-03-31"), "quarterly")
  expect_equal(state_type("2023-01-01", "2023-01-31"), "momthly")
})
