test_that("mc_standdr", {
  is_skip <- TRUE
  if (!is_skip) {
    out <- mc_standdr(ss = c(40, 100), nrep = 50)
  }
  skip_if(is_skip, "Skip to save time.")
  expect_s3_class(out, "data.frame")
  expect_identical(dim(out),c(20L, 7L))
})
