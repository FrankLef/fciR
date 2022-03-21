test_that("backdr_dr", {
  ids <- c("EY0", "EY1")

  data(whatif2dat)
  out <- backdr_dr(whatif2dat, outcome.name = "vl4", exposure.name ="A",
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
