test_that("sim_doublewhatif", {
  out <- sim_doublewhatif(n = 10)
  expect_s3_class(out, class = "data.frame")
  expect_identical(dim(out), c(10L, 7L))
})
