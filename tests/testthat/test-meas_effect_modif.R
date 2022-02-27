test_that("meas_effect_modif", {
  data(recovery)
  df <- recovery
  out <- meas_effect_modif(recovery, formula = Y ~ `T` + M, R = 100)
  out_id <- paste(out$estimator, out$group)

  data(fci_tbl_04_02)
  target <- fci_tbl_04_02
  target_id <- paste(target$estimator, target$group)

  ids <- match(target_id, out_id)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
