test_that("backdr_dr_bad", {
  data(whatif2dat)
  out <- backdr_dr_bad(whatif2dat, formula = vl4 ~ A + lvlcont0, R = 500)
  # print(out)

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.04)
})
