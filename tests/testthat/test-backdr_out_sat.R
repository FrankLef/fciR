test_that("backdr_out_sat", {
  ids <- c("EY0", "EY1", "RD", "RR")

  data(whatifdat)
  out <- backdr_out_sat(whatifdat, formula = Y ~ A * H,
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
  expect_lt(check, 0.005)
})

test_that("backdr_out_sat: With ATT", {
  ids <- c("EY0", "EY1", "RD", "RR")

  data(whatifdat)
  out <- backdr_out_sat(whatifdat, formula = Y ~ A * H,
                        exposure.name = "A", confound.names = "H", att = TRUE)
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
  expect_lt(check, 0.005)
})
