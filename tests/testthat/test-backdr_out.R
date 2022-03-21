test_that("backdr_out", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  out <- backdr_out(whatif2dat, outcome.name = "vl4", exposure.name ="A",
                    confound.names = "lvlcont0")
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.01)
})

test_that("backdr_out: Boot", {
  data(whatif2dat)
  out <- boot_est(data = whatif2dat, func = backdr_out,
                  R = 100, conf = 0.95,
                  outcome.name = "vl4", exposure.name = "A",
                  confound.names = "lvlcont0")

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})




test_that("backdr_outX", {
  data(whatif2dat)
  out <- backdr_outX(whatif2dat, formula = vl4 ~ A + lvlcont0)

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  skip("Deprecated")
  expect_lt(check, 0.01)
})
