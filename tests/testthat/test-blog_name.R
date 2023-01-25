test_that("code name", {
  expect_equal(blog_name("e", gap = 1), "F")
  expect_equal(blog_name("eg", gap = 2), "GI")
})
