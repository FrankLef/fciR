test_that("inst_loglinear", {
  ids <- c("RD" = 3, "logRR" = 6, "logOR" = 9)

  data("doublewhatifdat")
  out <- instr_loglinear(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
              instrument.name = "T")
  out <- out[out$term %in% names(ids), ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_09_01)
  target <- fci_tbl_09_01
  target <- target[ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.005)
})
