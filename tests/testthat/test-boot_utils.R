test_that("boot_run: Bootstrapping with base R", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_run(data = df, func = prob_lmod, times = 1000, alpha = 0.05,
                    formula = Y ~ `T` + A + H,
                    condition.names = c("T", "A"))
    out$.estimate <- round(out$.estimate, 4)

    # cat("\n", "out", "\n")
    # print(out)
    # cat("\n")

    target <- data.frame(
      term = "logitP",
      .lower = -3.14,
      .estimate = -2.3135,
      .upper = -1.28,
      .alpha = 0.05,
      .method = "norm"
    )

    # cat("\n", "target", "\n")
    # print(target)
    # cat("\n")
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_identical(dim(out), c(1L, 6L))
  expect_lt(abs(out$.estimate - target$.estimate), 0.01)
  expect_lt(abs(out$.lower - target$.lower), 0.025)
  expect_lt(abs(out$.upper - target$.upper), 0.025)
})


test_that("boot_run_td: Bootstrapping with tidyverse", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_run_td(data = df, func = prob_lmod_td, times = 1000, alpha = 0.05,
                       formula = Y ~ `T` + A + H,
                       condition.names = c("T", "A")) |>
      suppressWarnings()
    # cat("\n", "out", "\n")
    # print(out)
    # cat("\n")

    target <- data.frame(
      term = "logitP", .lower = -3.39, .estimate = -2.39,
      .upper = -1.57, .alpha = 0.05, .method = "percentile")
    # cat("\n", "target", "\n")
    # print(target)
    # cat("\n")
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_identical(dim(out), c(1L, 6L))
  expect_lt(abs(out$.estimate - target$.estimate), 0.01)
  expect_lt(abs(out$.lower - target$.lower), 0.025)
  expect_lt(abs(out$.upper - target$.upper), 0.025)
})


test_that("boot_est: Process the estimate obtained by bootstrapping", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_est(data = df, func = prob_lmod, times = 1000, alpha = 0.05,
                    transf = "expit",
                    formula = Y ~ `T` + A + H,
                    condition.names = c("T", "A"))
    # cat("\n", "out", "\n")
    # print(out)
    # cat("\n")

    target <- data.frame(
      "term" = "P", ".lower" = plogis(-3.39), ".estimate" = plogis(-2.39),
      ".upper" = plogis(-1.57), ".alpha" = 0.05, ".method" = "norm")
    # cat("\n", "target", "\n")
    # print(target)
    # cat("\n")
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_identical(dim(out), c(1L, 6L))
  expect_lt(abs(out$.estimate - target$.estimate), 0.01)
  expect_lt(abs(out$.lower - target$.lower), 0.025)
  expect_lt(abs(out$.upper - target$.upper), 0.05)
})
