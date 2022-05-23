test_that("meas_effect_cond", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_cond(data, formula = Y ~ `T` + A + H,
                          exposure.name = "T", condition.names = "H")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c("P0" = 0.77214651, "P1" = 0.73008627, "RD" = -0.04206024,
              "logRR" = -0.05601160, "logRR*" = -0.16939955,
              "logOR" = -0.22541115)

  expect_identical(names(out), names(target))
  expect_lt(sum(abs(out - target)), 1e-6)
})
