test_that("backdr_out_np", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})

test_that("backdr_out_np: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H",
                       att = TRUE)
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})


test_that("backdr_out_npr", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_npr(whatifdat, outcome.name = "Y",
                       exposure.name = "A", confound.names = "H",
                       interactions = list(c("A", "H")))
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_01)
  target <- fci_tbl_06_01
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})

test_that("backdr_out_npr: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_out_npr(whatifdat, outcome.name = "Y",
                        exposure.name = "A", confound.names = "H",
                        interactions = list(c("A", "H")), att = TRUE)
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})
