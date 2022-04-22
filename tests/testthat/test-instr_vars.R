test_that("instr_vars", {

  data(whatif2dat)
  out <- instr_vars(whatifdat, outcome.name = "Y", exposure.name = "A",
                    instrument.name = "T")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c(0.007352941, 0.27777778)
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 1e-6)
})
