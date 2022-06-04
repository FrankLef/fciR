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
  est <- c("val0" = val0, "val1" = val1, "RD" = val1 - val0,
           "logRR" = 0.6931, "logRR*" = 0.4055, "logOR" = 1.0986)
  df <- data.frame(
    name = c("EY0", "EY1", "RD", "logRR", "logRR*", "logOR"),
    est = est,
    lci = est * 0.75,
    uci = est * 1.25
  )
  out <- effect_exp(df)

  target <- df
  target$name <- c("EY0", "EY1", "RD", "RR", "RR*", "OR")
  target$est <- c(target$est[1], target$est[2], target$est[3],
                  exp(target$est[4]), exp(target$est[5]), exp(target$est[6]))
  target$lci <- c(target$lci[1], target$lci[2], target$lci[3],
                  exp(target$lci[4]), exp(target$lci[5]), exp(target$lci[6]))
  target$uci <- c(target$uci[1], target$uci[2], target$uci[3],
                  exp(target$uci[4]), exp(target$uci[5]), exp(target$uci[6]))

  expect_identical(out, target)
})
