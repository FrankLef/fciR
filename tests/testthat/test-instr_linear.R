test_that("inst_linear", {

  data("doublewhatifdat")
  out <- instr_linear(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
              instrument.name = "T")
  out <- out[c("RD", "logRR", "logOR")]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_09_01)
  target <- fci_tbl_09_01
  target <- target$est[c(2, 5, 8)]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  # skip("TODO")
  expect_lt(check, 1e-3)
})
