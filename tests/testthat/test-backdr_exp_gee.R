test_that("backdr_exp_gee", {
  is_skip <- TRUE
  if(!is_skip) {
    data(whatif2dat)
    out <- backdr_exp_gee(whatif2dat, formula = vl4 ~ A + lvlcont0, R = 500)

    data(fci_tbl_06_09)
    target <- fci_tbl_06_09

    ids <- match(target$name, out$name, nomatch = 0L)
    check <- sum(abs(out$est[ids] - target$est))
  }

  skip_if(is_skip, "Skip to save time.")
  expect_lt(check, 0.01)
})
