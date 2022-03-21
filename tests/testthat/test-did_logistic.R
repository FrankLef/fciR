test_that("did_logistic", {
  ids <- c("EY0A1")

  data(doublewhatifdat)
  out <- did_logistic(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
                    confound.names = "VL0",
                    names_to = "var", timevar = "time")
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_07_02)
  target <- fci_tbl_07_02
  target <- target[target$method == "Logistic", ]
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})
