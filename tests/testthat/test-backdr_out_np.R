test_that("backdr_out_np", {
  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome = Y, exposure = A, confound = H,
                       R = 200)

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})

test_that("backdr_out_np: With ATT", {
  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome = Y, exposure = A, confound = H,
                       att = TRUE, R = 200)
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  # cat("\n")
  # print(target)
  # cat("\n")

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})


test_that("backdr_out_npr", {
 data(whatifdat)
 out <- backdr_out_npr(whatifdat, formula = Y ~ A + H + A:H, R = 500)

 data(fci_tbl_06_01)
 target <- fci_tbl_06_01
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 expect_lt(check, 0.01)
})


test_that("backdr_out_npr: With ATT", {
 data(whatifdat)
 out <- backdr_out_npr(whatifdat, formula = Y ~ A + H + A:H, att = TRUE, R = 500)

 data(fci_tbl_06_04)
 target <- fci_tbl_06_04
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 expect_lt(check, 0.01)
})
