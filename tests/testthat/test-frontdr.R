test_that("frontdr_np", {
  ids <- c("EY0", "EY1")

  data(fci_sim_08_01)

  out <- frontdr_np(fci_sim_08_01, outcome.name = "Y", exposure.name = "A",
                    confound.names = "S")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    est = c(0.055847, 0.185367, 0.129520, 3.319195, 1.158992, 3.846921))
  target <- target$est[target$name %in% target]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 1e-6)
})
