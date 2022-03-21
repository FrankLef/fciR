test_that("prob_lmod", {
  data("whatifdat")
  data <- whatifdat

  out <- prob_lmod(data,
                   outcome.name = "Y",
                   input.names = c("T", "A", "H"),
                   condition.names = c("T", "A"))
  out <- round(out, 6)

  target <- c("logitP" = round(qlogis(0.09001343), 6))

  expect_identical(out, target)
})

test_that("prob_lmod: Boot", {
  data("whatifdat")
  data <- whatifdat

  out <- boot_est(data, func = prob_lmod, R = 500, conf = 0.95,
                  inv = "expit", evars = "logit",
                  outcome.name = "Y",
                  input.names = c("T", "A", "H"),
                  condition.names = c("T", "A"))
  # cat("\n")
  # print(out)
  # cat("\n")

  expect_lt(abs(out["est"] - 0.09001343), 0.02)
  expect_lt(abs(out["lci"] - 0.04190813), 0.02)
  expect_lt(abs(out["uci"] - 0.208577), 0.02)
})
