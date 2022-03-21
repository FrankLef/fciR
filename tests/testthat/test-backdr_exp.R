test_that("backdr_exp", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  out <- backdr_exp(whatif2dat, outcome.name = "vl4", exposure.name = "A",
                    confound.names = "lvlcont0")
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})


test_that("backdr_expX", {
  data(whatif2dat)
  out <- backdr_expX(whatif2dat, formula = vl4 ~ A + lvlcont0)

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09

  ids <- match(target$name, out$name, nomatch = 0L)
  skip("Deprecated")
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
