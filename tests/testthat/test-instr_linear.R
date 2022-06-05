test_that("inst_linear", {
  ids <- c("RD" = 2, "logRR" = 5, "logOR" = 8)

  data("doublewhatifdat")
  out <- instr_linear(doublewhatifdat, outcome.name = "VL1", exposure.name = "A",
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
  expect_lt(check, 1e-3)
})
