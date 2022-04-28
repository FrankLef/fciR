test_that("inst_logistic", {

  data("doublewhatifdat")
  out <- instr_logistic(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
              instrument.name = "T")
  out <- out[c("RD", "logRR", "logOR")]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_09_01)
  target <- fci_tbl_09_01
  target <- target$est[c(4, 7, 10)]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  # skip("TODO")
  expect_lt(check, 0.001)
})
