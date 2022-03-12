test_that("backdr_exp_np", {
  data(whatifdat)
  out <- backdr_exp_np(whatifdat, outcome = Y, exposure = A, confound = H,
                       R = 200)
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01
  # cat("\n")
  # print(target)
  # cat("\n")

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})

test_that("backdr_exp_np: With ATT", {
  data(whatifdat)
  out <- backdr_exp_np(whatifdat, outcome = Y, exposure = A, confound = H,
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
  # print(check)
  expect_lt(check, 0.01)
})

test_that("backdr_exp_bb", {
  data(mortality_long)
  out <- backdr_exp_bb(mortality_long, formula = Y ~ `T` + H, weights = "n")

  target <- list("EY0" = 0.0078399, "EY1" = 0.0069952, "EY0T1" = 0.010176)

  check <- abs(out$EY0 - target$EY0) + abs(out$EY1 - target$EY1) +
    abs(out$EY0T1 - target$EY0T1)
  expect_lt(check, 1e-6)
})
