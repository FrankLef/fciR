test_that("backdr_out", {
  data(whatif2dat)
  out <- backdr_out(whatif2dat, formula = vl4 ~ A + lvlcont0)

  data(fci_tbl_06_07)
  target <- fci_tbl_06_07

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
