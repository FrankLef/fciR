test_that("backdr_exp_gee", {
  ids <- c("EY0", "EY1", "RD")

  data(whatif2dat)
  out <- backdr_exp_gee(whatif2dat, formula = vl4 ~ A + lvlcont0,
                        exposure.name ="A", confound.names = "lvlcont0")
  out <- out[out$term %in% ids, ]
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_06_09)
  target <- fci_tbl_06_09
  target <- target[target$term %in% ids, ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$estimate - target$.estimate))
  expect_lt(check, 0.01)
})
