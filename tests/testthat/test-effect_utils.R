test_that("effect_measures", {
  val0 <- 0.25
  val1 <- 0.5
  out <- effect_measures(val0, val1, log = TRUE)
  out <- round(out, 4)

  target <- c("val0" = val0, "val1" = val1, "RD" = val1 - val0,
              "logRR" = 0.6931, "logRR*" = 0.4055, "logOR" = 1.0986)
  expect_identical(out, target)

  out <- effect_measures(val0, val1, log = FALSE)

  target <- c("val0" = val0, "val1" = val1, "RD" = val1 - val0,
              "RR" = 2, "RR*" = 1.5, "OR" = 3)
  expect_identical(out, target)
})

test_that("effect_vars", {

  # The standard effect variables
  out <- effect_vars()
  target <- c("RR" = "logRR", "RR*"  = "logRR*", "OR" = "logOR")
  expect_identical(out, target)

  # the effect variables for modifiers
  out <- effect_vars("modifier")
  target <- c("RR.M0" = "logRR.M0", "RR.M1" = "logRR.M1",
              "RR.diff"  = "logRR.diff", "RR*.M0"  = "logRR*.M0",
              "RR*.M1"  = "logRR*.M1", "RR*.diff" = "logRR*.diff",
              "OR.M0" = "logOR.M0", "OR.M1" = "logOR.M1",
              "OR.diff" = "logOR.diff")
  expect_identical(out, target)

  # the effect variable for logitP
  out <- effect_vars("logit")
  target <- c("P" = "logitP")
  expect_identical(out, target)
})


test_that("effect_exp", {
  val0 <- 0.25
  val1 <- 0.5
  est <- c(val0, val1, val1 - val0, 0.6931, 0.4055, 1.0986)
  df <- data.frame(
    term = c("EY0", "EY1", "RD", "logRR", "logRR*", "logOR"),
    .lower = est * 0.75,
    .estimate = est,
    .upper = est * 1.25,
    .alpha = 0.05,
    .method = "norm"
  )
  out <- effect_exp(df)
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- df
  target$term <- c("EY0", "EY1", "RD", "RR", "RR*", "OR")
  target$.estimate <- c(target$.estimate[1],
                        target$.estimate[2],
                        target$.estimate[3],
                        exp(target$.estimate[4]),
                        exp(target$.estimate[5]),
                        exp(target$.estimate[6]))
  target$.lower <- c(target$.lower[1],
                     target$.lower[2],
                     target$.lower[3],
                     exp(target$.lower[4]),
                     exp(target$.lower[5]),
                     exp(target$.lower[6]))
  target$.upper <- c(target$.upper[1],
                     target$.upper[2],
                     target$.upper[3],
                     exp(target$.upper[4]),
                     exp(target$.upper[5]),
                     exp(target$.upper[6]))

  expect_identical(out, target)
})
