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
