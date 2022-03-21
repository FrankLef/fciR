test_that("backdr_out_np", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H")
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})

test_that("backdr_out_np: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H",
                       att = TRUE)
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})


test_that("backdr_out_npr", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_npr(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H",
                       interactions = list(c("A", "H")))
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})

test_that("backdr_out_npr: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_npr(whatifdat, outcome.name = "Y",
                        exposure.name = "A", confound.names = "H",
                        interactions = list(c("A", "H")), att = TRUE)
  out <- out[ids]
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  target <- target$est[target$name %in% ids]
  # cat("\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out - target))
  expect_lt(check, 0.001)
})


test_that("backdr_out_npX", {
  data(whatifdat)
  out <- backdr_out_npX(whatifdat, outcome = Y, exposure = A, confound = H,
                       R = 200)

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01

  ids <- match(target$name, out$name, nomatch = 0L)
  skip("Deprecated")
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})

test_that("backdr_out_npX: With ATT", {
  data(whatifdat)
  out <- backdr_out_npX(whatifdat, outcome = Y, exposure = A, confound = H,
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
  skip("Deprecated")
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})


test_that("backdr_out_nprX", {
 data(whatifdat)
 out <- backdr_out_nprX(whatifdat, formula = Y ~ A + H + A:H, R = 500)

 data(fci_tbl_06_01)
 target <- fci_tbl_06_01
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 skip("Deprecated")
 expect_lt(check, 0.01)
})


test_that("backdr_out_nprX: With ATT", {
 data(whatifdat)
 out <- backdr_out_nprX(whatifdat, formula = Y ~ A + H + A:H, att = TRUE, R = 500)

 data(fci_tbl_06_04)
 target <- fci_tbl_06_04
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 skip("Deprecated")
 expect_lt(check, 0.01)
})
