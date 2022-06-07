test_that("backdr_exp_np", {
  ids <- c("EY0", "EY1", "RD")

  data(whatifdat)
  out <- backdr_exp_np(whatifdat, formula = Y ~ A + H, exposure.name = "A")
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

test_that("backdr_exp_np: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_exp_np(whatifdat, formula = Y ~ A + H, exposure.name = "A",
                       att = TRUE)
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_04)
  target <- fci_tbl_06_04
  target <- target$est[target$term %in% ids]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.001)
})
