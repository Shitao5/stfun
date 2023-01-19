test_that("remove link and tags", {
  Sys.setenv(CLIPR_ALLOW=TRUE)
  clipr::write_clip("A [[nice]] day. #happy",
                    allow_non_interactive = TRUE)
  expect_equal({
    remove_tag()
    clipr::read_clip()
  }, "A nice day.")
})
