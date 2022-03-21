test_that("meas_effect_modif", {
  data(recovery)
  df <- recovery
  out <- meas_effect_modif(recovery, outcome.name = "Y", exposure.name = "T",
                           modifier.name = "M")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c("EYT0.M0" = 0.78367235, "EYT0.M1" = 0.59297218,
              "EYT1.M0" = 0.79325843, "EYT1.M1" = 0.73456790,
              "RD.M0" = 0.00958608, "RD.M1" = 0.14159572,
              "logRR.M0" = 0.01215804, "logRR.M1" = 0.21413495,
              "logRR*.M0" = 0.04532459, "logRR*.M1" = 0.42752247,
              "logOR.M0" = 0.05748264, "logOR.M1" = 0.64165742,
              "EYT0.diff" = -0.19070017, "EYT1.diff" = -0.05869053,
              "RD.diff" = 0.13200964, "logRR.diff" = 0.20197691,
              "logRR*.diff" = 0.38219488, "logOR.diff" = 0.58417478)

  expect_identical(names(out), names(target))
  expect_lt(sum(abs(out - target)), 1e-4)
})

test_that("meas_effect_modif: Boot", {
  data(recovery)
  df <- recovery

  out <- boot_est(data = recovery, func = meas_effect_modif,
                  R = 100, conf = 0.95,
                  inv = "exp", evars = "modifier",
                  outcome.name = "Y", exposure.name = "T",
                  modifier.name = "M")
  # cat("\n")
  # print(out)
  # cat("\n")

  data(fci_tbl_04_02)
  target <- fci_tbl_04_02
  target_id <- paste(target$estimator, target$group, sep = ".")
  # cat("\n")
  # print(target)
  # cat("\n")

  ids <- match(target_id, out$name)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})
