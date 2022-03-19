test_that("did_logistic", {
  data(doublewhatifdat)
  df <- doublewhatifdat
  out <- did_logistic(df, outcomes = c("VL0", "VL1"), outcome = "Y",
                      treatment = "A",
                      names_to = "var", timevar = "time", R = 50)

  data(fci_tbl_07_02)
  target <- fci_tbl_07_02
  target <- target[target$method == "Logistic", ]

  ids <- match(target$name, out$name, nomatch = 0L)

  # cat("\n", "Out: ", print(out$est[ids]), "\n")
  # cat("\n", "Target: ", print(target$est), "\n")

  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
