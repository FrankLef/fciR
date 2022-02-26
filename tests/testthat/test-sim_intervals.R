test_that("sim_intervals", {
  # out <- round(sim_intervals(nsim = 100, n = 100), 2)
  out <- sim_intervals(nsim = 1000, n = 1000)
  out <- lapply(out, function(x) round(x, 2))
  test <- list("bad" = 0.8374, "good" = 0.948)
  test <- lapply(test, function(x) round(x, 2))

  check <- abs(out[[1]] - test[[1]]) + abs(out[[2]] - test[[2]])
  expect_true(check < 0.03)
})
