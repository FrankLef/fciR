test_that("backdr_exp_gee", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  out <- backdr_exp_gee(whatif2dat, outcome.name = "vl4", exposure.name ="A",
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
  expect_lt(check, 0.01)
})


test_that("backdr_exp_geeX", {
  is_skip <- TRUE
  if(!is_skip) {
    data(whatif2dat)
    out <- backdr_exp_geeX(whatif2dat, formula = vl4 ~ A + lvlcont0, R = 500)

    data(fci_tbl_06_09)
    target <- fci_tbl_06_09

    ids <- match(target$name, out$name, nomatch = 0L)
    check <- sum(abs(out$est[ids] - target$est))
  }

  skip("Deprecated")
  # check <- sum(abs(out - target))
  expect_lt(check, 0.01)
})
