test_that("instr_vars", {

  data(whatifdat)
  df <- whatifdat
  out <- instr_vars(df, formula = Y ~ A + `T`, exposure.name = "A")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c("ITT" = 0.007352941, "IV" = 0.27777778)
  target <- data.frame(
    term = c("ITT", "IV"),
    estimate = c(0.007352941, 0.27777778),
    std.err = NA_real_
  )
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$estimate))
  expect_lt(check, 1e-6)
  expect_identical(names(out), names(target))
})
