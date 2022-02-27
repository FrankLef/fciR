test_that("backdr_exp_np", {
  data(mortality_long)
  out <- backdr_exp_np(mortality_long, formula = Y ~ `T` + H, weights = "n")

  target <- list("EY0" = 0.0078399, "EY1" = 0.0069952, "EY0T1" = 0.010176)

  check <- abs(out$EY0 - target$EY0) + abs(out$EY1 - target$EY1) +
    abs(out$EY0T1 - target$EY0T1)
  expect_lt(check, 1e-6)
})
