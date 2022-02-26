test_that("formula2vars", {
  out <- formula2vars(Y ~ `T` + H + `T` * H)

  target <- list("y" = "Y", "t" = "T", "ind" = c("T", "H"), "x0" = "(Intercept)")
  target <- list("y" = "Y", "t" = "T", "h" = "H", "ht" = c("H", "T:H"),
                 "ind" = c("T", "H", "T:H"), "x0" = "(Intercept)")
  expect_identical(out, target)
})

test_that("formula2vars: minimal", {
  out <- formula2vars(Y ~ `T`)

  target <- list("y" = "Y", "t" = "T", "ind" = "T", "x0" = "(Intercept)")
  expect_identical(out, target)
})

test_that("calc_effect_measures", {
  val0 <- 0.25
  val1 <- 0.5
  out <- calc_effect_measures(val0, val1, log = TRUE)
  out <- round(out, 4)

  target <- c("val0" = 0.25, "val1" = 0.5, "RD" = val1 - val0,
              "logRR" = 0.6931, "logRR*" = 0.4055, "logOR" = 1.0986)
  expect_identical(out, target)

  out <- calc_effect_measures(val0, val1, log = FALSE)
  print(out)
  target <- c("val0" = 0.25, "val1" = 0.5, "RD" = val1 - val0,
              "RR" = 2, "RR*" = 1.5, "OR" = 3)
  expect_identical(out, target)
  #'
})
