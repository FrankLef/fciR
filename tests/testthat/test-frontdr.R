test_that("est_frontdr_np", {
  data(fci_sim_08_01)
  df <- fci_sim_08_01

  out <- est_frontdr_np(df, outcome = "Y", exposure = "A", surrogate = "S")

  target <- c("EY0" = 0.055847, "EY1" = 0.185367)

  check <- sum(abs(out - target))
  expect_lt(check, 1e-6)
})

test_that("frontdr_np", {
  data(fci_sim_08_01)
  df <- fci_sim_08_01

  out <- frontdr_np(df, outcome = "Y", exposure = "A", surrogate = "S", R = 100)
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    est = c(0.055847, 0.185367, 0.129520, 3.319195, 1.158992, 3.846921))

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 1e-6)
})
