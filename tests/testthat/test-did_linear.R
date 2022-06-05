test_that("did_linear", {
  ids <- c("EY0A1")

  data(doublewhatifdat)
  out <- did_linear(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
                    confound.names = "VL0",
                    names_to = "var", timevar = "time")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_07_02)
  target <- fci_tbl_07_02
  target <- target[target$method == "Linear", ]
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})
