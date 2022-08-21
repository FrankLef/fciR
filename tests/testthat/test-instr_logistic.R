test_that("inst_logistic", {
  ids <- c("RD" = 4, "logRR" = 7, "logOR" = 10)

  data("doublewhatifdat")
  out <- instr_logistic(doublewhatifdat, formula = VL1 ~ A * `T`,
                        exposure.name = "A", instrument.name = "T")
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
  expect_lt(check, 0.001)
})
