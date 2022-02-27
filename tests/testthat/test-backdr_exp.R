test_that("backdr_exp", {
  data(whatif2dat)
  out <- backdr_exp(whatif2dat, formula = vl4 ~ A + lvlcont0)

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
