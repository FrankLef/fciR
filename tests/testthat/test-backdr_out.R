test_that("backdr_out", {
  ids <- c("EY0", "EY1", "RD")

  data(whatif2dat)
  out <- backdr_out(whatif2dat, formula = vl4 ~ A + lvlcont0, exposure.name ="A")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.01)
})


test_that("backdr_out: Bootstrapping estimate", {
  data(whatif2dat)
  out <- boot_est(data = whatif2dat, func = backdr_out,
                  times = 100, alpha = 0.05, seed = NULL, transf = "exp",
                  formula = vl4 ~ A + lvlcont0, exposure.name ="A")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  ids <- match(target$.estimate, out$.estimate, nomatch = 0L)
  check <- sum(abs(out$.estimate[ids] - target$.estimate))
  expect_lt(check, 0.01)
})
