test_that("meas_effect_cond", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_cond(data, outcome.name = "Y",
                          exposure.name = "T",
                          confound.names = c("A", "H"),
                          condition.names = c("H"))
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c("P0" = 0.77214651, "P1" = 0.73008627, "RD" = -0.04206024,
              "logRR" = -0.05601160, "logRR*" = -0.16939955,
              "logOR" = -0.22541115)

  expect_identical(names(out), names(target))
  expect_lt(sum(abs(out - target)), 1e-6)
})


test_that("meas_effect_cond: Boot", {
  data("whatifdat")
  data <- whatifdat

  out <- boot_est(data, func = meas_effect_cond, R = 100, conf = 0.95,
                  outcome.name = "Y", exposure.name = "T",
                  confound.names = c("A", "H"),
                  condition.names = c("H"))
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    est = c(0.77214651, 0.73008627, -0.04206024,
            0.94552816, 0.84417155, 0.79818797),
    conf = 0.95,
    lci = c(0.6295037, 0.5829942, -0.1764233,
            0.7857248, 0.4823648, 0.3833990),
    uci = c(0.92478949, 0.88452600, 0.08965033,
            1.13627178, 1.49281056, 1.67680309))

  skip("Deprecated")
  expect_identical(dim(out), c(6L, 5L))
  expect_lt(sum(abs(out$est - target$est)), 0.01)
  expect_lt(sum(abs(out$conf - target$conf)), 0.01)
  expect_lt(sum(abs(out$lci - target$lci)), 0.5)
  expect_lt(sum(abs(out$uci - target$uci)), 0.5)
})

test_that("meas_effect_condX", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_condX(data, formula = Y ~ `T` + A + H,
                   cond0 = Y ~ A + H,
                   cond1 = Y ~ `T` + A + H,
                   R = 1000)
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    est = c(0.65830834, 0.60595808, -0.05235026,
            0.92047760, 0.86714546, 0.79818797),
    conf = 0.95,
    lci = c(0.4798008, 0.4300890, -0.2525423,
            0.6619120, 0.4866379, 0.3267191),
    uci = c(0.8368685, 0.7784991, 0.1444611,
            1.2755695, 1.5653375, 1.9685380))

  skip("Deprecated")
  expect_identical(dim(out), c(6L, 5L))
  expect_lt(sum(abs(out$est - target$est)), 0.01)
  expect_lt(sum(abs(out$conf - target$conf)), 0.01)
  expect_lt(sum(abs(out$lci - target$lci)), 0.5)
  expect_lt(sum(abs(out$uci - target$uci)), 0.5)
})
