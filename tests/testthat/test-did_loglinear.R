test_that("did_loglinear", {
  ids <- c("EY0A1")

  data(doublewhatifdat)
  out <- did_loglinear(doublewhatifdat, formula = VL1 ~ A + VL0,
                       exposure.name = "A", names_to = "var", timevar = "time")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_07_02)
  target <- fci_tbl_07_02
  target <- target[target$method == "Loglinear", ]
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")


  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.01)
})
