test_that("meas_effect_uncond", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_uncond(data, outcome.name = "Y", input.names = "T")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c("P0" = 0.317647059, "P1" = 0.325, "RD" = 0.007352941,
              "logRR" = 0.022884294, "logRR*" = 0.010834342,
              "logOR" = 0.033718636)

  expect_identical(names(out), c("P0", "P1", "RD", "logRR", "logRR*", "logOR"))
  expect_lt(sum(abs(out - target)), 1e-6)
})


test_that("meas_effect_uncond: Boot", {
  data("whatifdat")
  data <- whatifdat

  out <- boot_est(data, func = meas_effect_uncond, R = 100, conf = 0.95,
                  outcome.name = "Y", input.names = "T")
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    est = c(0.317647059, 0.325, 0.007352941,
            1.023148148, 1.010893246, 1.034293553),
    conf = 0.95,
    lci = c(0.2243634, 0.2185020, -0.1399316,
            0.6404295, 0.8109253, 0.5204578),
    uci = c(0.4161168, 0.4306618, 0.1486153,
            1.6092094, 1.2473330, 2.0029108))

  expect_identical(dim(out), c(6L, 5L))
  expect_lt(sum(abs(out$est - target$est)), 0.01)
  expect_lt(sum(abs(out$conf - target$conf)), 0.01)
  expect_lt(sum(abs(out$lci - target$lci)), 0.5)
  expect_lt(sum(abs(out$uci - target$uci)), 0.5)
})


test_that("meas_effect_uncondX", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_uncondX(data, Y ~ `T`, R = 100)
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    name = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    est = c(0.317647059, 0.325, 0.007352941,
            1.023148148, 1.010893246, 1.034293553),
    conf = 0.95,
    lci = c(0.2243634, 0.2185020, -0.1399316,
            0.6404295, 0.8109253, 0.5204578),
    uci = c(0.4161168, 0.4306618, 0.1486153,
            1.6092094, 1.2473330, 2.0029108))

  skip("Deprecated")
  expect_identical(dim(out), c(6L, 5L))
  expect_lt(sum(abs(out$est - target$est)), 0.01)
  expect_lt(sum(abs(out$conf - target$conf)), 0.01)
  expect_lt(sum(abs(out$lci - target$lci)), 0.5)
  expect_lt(sum(abs(out$uci - target$uci)), 0.5)
})
