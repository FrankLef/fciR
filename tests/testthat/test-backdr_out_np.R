test_that("backdr_out_np", {
  data(whatifdat)
  out <- backdr_out_np(whatifdat, formula = Y ~ A + H + A:H)

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})


test_that("backdr_out_np: With ATT", {
  data(whatifdat)
  out <- backdr_out_np(whatifdat, formula = Y ~ A + H + A:H, att = TRUE)

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
