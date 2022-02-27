test_that("effect_measures", {
  val0 <- 0.25
  val1 <- 0.5
  out <- effect_measures(val0, val1, log = TRUE)
  out <- round(out, 4)

  target <- c("val0" = 0.25, "val1" = 0.5, "RD" = val1 - val0,
              "logRR" = 0.6931, "logRR*" = 0.4055, "logOR" = 1.0986)
  expect_identical(out, target)

  out <- effect_measures(val0, val1, log = FALSE)

  target <- c("val0" = 0.25, "val1" = 0.5, "RD" = val1 - val0,
              "RR" = 2, "RR*" = 1.5, "OR" = 3)
  expect_identical(out, target)
})
