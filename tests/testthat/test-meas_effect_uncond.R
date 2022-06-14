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
                  seed = 1234, transf = "exp", formula = Y ~ `T`)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("P0", "P1", "RD", "RR", "RR*", "OR"),
    .lower = c(0.228, 0.203, -0.178,
               0.565, 0.763, 0.437),
    .estimate = c(0.317, 0.329, 0.0119,
                  1.04, 1.02, 1.05),
    .upper = c(0.417, 0.438, 0.165,
               1.67, 1.28, 2.13),
    .alpha = 0.05,
    .method = "norm")
  # NOTE: For some reason the rows are sorted differently by boot_run
  target <- target[match(out$term, target$term, nomatch = 0), ]
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(names(out), names(target))
  expect_identical(dim(out), c(6L, 6L))
  expect_identical(out$term, target$term)
  expect_lt(sum(abs(out$.estimate - target$.estimate)), 0.015)
  expect_lt(sum(abs(out$.lower - target$.lower)), 0.01)
  expect_lt(sum(abs(out$.upper - target$.upper)), 0.01)
})
