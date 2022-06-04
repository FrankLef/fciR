test_that("meas_effect_uncond", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_uncond(data, formula = Y ~ `T`)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c("P0" = 0.317647059, "P1" = 0.325, "RD" = 0.007352941,
              "logRR" = 0.022884294, "logRR*" = 0.010834342,
              "logOR" = 0.033718636)
  target <- data.frame(
    term = names(target),
    estimate = unname(target),
    std.err = NA_real_
  )
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(names(out), names(target))
  expect_identical(out$term, target$term)
  expect_lt(sum(abs(out$estimate - target$estimate)), 1e-6)
})


test_that("meas_effect_uncond: Boot", {
  data("whatifdat")
  data <- whatifdat

  out <- boot_est(data, func = meas_effect_uncond, times = 100, alpha = 0.05,
                  formula = Y ~ `T`)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    .lower = c(0.2243634, 0.2185020, -0.1399316,
               0.6404295, 0.8109253, 0.5204578),
    .estimate = c(0.317647059, 0.325, 0.007352941,
                  1.023148148, 1.010893246, 1.034293553),
    .upper = c(0.4161168, 0.4306618, 0.1486153,
               1.6092094, 1.2473330, 2.0029108),
    .alpha = 0.05,
    .method = "norm")
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(names(out), names(target))
  expect_identical(dim(out), c(6L, 6L))
  expect_identical(out$term, target$term)
  expect_lt(sum(abs(out$.estimate - target$.estimate)), 0.01)
  expect_lt(sum(abs(out$.lower - target$.lower)), 0.3)
  expect_lt(sum(abs(out$.upper - target$.upper)), 0.3)
})
