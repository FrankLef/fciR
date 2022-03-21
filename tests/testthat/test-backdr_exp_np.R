test_that("backdr_exp_np", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_exp_np(whatifdat, outcome.name = "Y", exposure.name = "A",
                       confound.names = "H")
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
  expect_lt(check, 0.01)
})

test_that("backdr_exp_np: With ATT", {
  ids <- c("EY0", "EY1")

  data(whatifdat)
  out <- backdr_exp_np(whatifdat, outcome.name = "Y", exposure.name = "A",
                       confound.names = "H", att = TRUE)
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
  expect_lt(check, 0.01)
})

test_that("backdr_exp_bb", {

  data(mortality_long)
  out <- backdr_exp_bb(mortality_long, outcome.name = "Y", exposure.name = "T",
                       confound.names = "H", weights = "n")

  target <- list("EY0" = 0.0078399, "EY1" = 0.0069952, "EY0T1" = 0.010176)

  check <- abs(out$EY0 - target$EY0) + abs(out$EY1 - target$EY1) +
    abs(out$EY0T1 - target$EY0T1)
  expect_lt(check, 1e-6)
})

test_that("backdr_exp_bb: Boot", {
  data(mortality_long)
  is_skip <- TRUE
  if (!is_skip) {
    out <- fciR::boot_est(data = mortdat, func = fciR::backdr_exp_bb,
                          R = 100, conf = 0.95,
                          outcome.name = "Y", exposure.name = "T",
                          confound.names = "H", weights = "n")

    target <- list("EY0" = 0.0078399, "EY1" = 0.0069952, "EY0T1" = 0.010176)

    check <- abs(out$EY0 - target$EY0) + abs(out$EY1 - target$EY1) +
      abs(out$EY0T1 - target$EY0T1)
  }
  skip_if(is_skip, "Cannot be run with boot_est: Error message to resolve.")
  expect_lt(check, 1e-6)
})


test_that("backdr_exp_bbX", {
  data(mortality_long)
  out <- backdr_exp_bbX(mortality_long, formula = Y ~ `T` + H, weights = "n")

  target <- list("EY0" = 0.0078399, "EY1" = 0.0069952, "EY0T1" = 0.010176)

  check <- abs(out$EY0 - target$EY0) + abs(out$EY1 - target$EY1) +
    abs(out$EY0T1 - target$EY0T1)
  skip("Deprecated")
  expect_lt(check, 1e-6)
})
