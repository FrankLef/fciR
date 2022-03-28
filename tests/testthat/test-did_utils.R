test_that("did_longer", {
  data(doublewhatifdat)
  df <- doublewhatifdat
  out <- did_longer(df, outcomes = c("VL0", "VL1"),
                   outcome = "Y", names_to = "var", timevar = "time")
  # cat("\n In:\n")
  # str(df)
  # cat("\n Out:\n")
  # str(out)
  # cat("\n")

  expect_identical(dim(out), c(2000L, 8L))
})
