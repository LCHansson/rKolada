
context("Metadata")
munic <- get_kld_metadata(entity = "municipality", cache = FALSE)

test_that("metadata downloads correctly", {
  expect_true(inherits(munic, "tbl_df"))
  expect_true(nrow(munic) > 0)
})