test_that("backdr_out", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  # out <- backdr_out(whatif2dat, outcome.name = "vl4", exposure.name ="A",
  #                   confound.names = "lvlcont0")
  out <- backdr_out(whatif2dat, formula = vl4 ~ A + lvlcont0, exposure.name ="A")
  out <- out$estimate[out$term %in% ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07
  target <- target$.estimate[target$term %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.01)
})

test_that("backdr_out: Bootstrapping", {
  data(whatif2dat)
  out <- boot_est(data = whatif2dat, func = backdr_out,
                  times = 100, alpha = 0.05,
                  formula = vl4 ~ A + lvlcont0, exposure.name ="A")
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07
  # cat("\n")
  # print(target)
  # cat("\n")

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$.estimate[ids] - target$est))
  expect_lt(check, 0.01)
})
