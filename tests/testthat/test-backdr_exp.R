test_that("backdr_exp", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  out <- backdr_exp(whatif2dat, outcome.name = "vl4", exposure.name = "A",
                    confound.names = "lvlcont0")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09
  target <- target$est[target$term %in% ids]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.01)
})
