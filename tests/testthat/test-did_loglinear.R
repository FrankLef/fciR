test_that("did_loglinear", {
  data(doublewhatifdat)
  df <- doublewhatifdat
  out <- did_loglinear(df, formula = Y ~ A, varsY = c("VL0", "VL1"), R = 50)

  data(fci_tbl_07_02)
  target <- fci_tbl_07_02
  target <- target[target$method == "Loglinear", ]

  ids <- match(target$name, out$name, nomatch = 0L)

  # cat("\n", "Out: ", print(out$est[ids]), "\n")
  # cat("\n", "Target: ", print(target$est), "\n")

  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})