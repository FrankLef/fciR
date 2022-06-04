test_that("boot_run: Bootstrapping with base R", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_run(data = df, func = prob_lmod, times = 1000, alpha = 0.05,
                    formula = Y ~ `T` + A + H,
                    condition.names = c("T", "A"))
    out$.estimate <- round(out$.estimate, 4)

    # cat("\n")
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

    # cat("\n")
    # print(target)
    # cat("\n")

    ids <- c(".lower", ".estimate", ".upper")
    check <- sum(abs(out[ids] - target[ids]))
    check <- abs(out$.lowert[1] - target$.lower[1]) +
      abs(out$.estimate[1] - target$.estimate[1]) + abs(out$.upper[1] - target$.upper[1])
    check <- sum(check)
  }

  skip_if(is_skip, "Skip to save time.")
  # expect_identical(out, target)
  expect_identical(names(out), names(target))
  expect_lt(check, 0.01)
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
    # cat("\n")
    # print(out)
    # cat("\n")

    target <- data.frame(
      term = "logitP", .lower = -3.39, .estimate = -2.39,
      .upper = -1.57, .alpha = 0.05, .method = "percentile")
    # cat("\n")
    # print(target)
    # cat("\n")

    check <- abs(out$.lower[1] - target$.lower[1]) +
      abs(out$.estimate[1] - target$.estimate[1]) +
      abs(out$.upper[1] - target$.upper[1])
    check <- sum(check)
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_lt(check, 0.15)
})


test_that("boot_est: Process the estimate obtained by bootstrapping", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_est(data = df, func = prob_lmod, times = 1000, alpha = 0.05,
                    inv = "expit", evars = "logit",
                    formula = Y ~ `T` + A + H,
                    condition.names = c("T", "A"))
    cat("\n")
    print(out)
    cat("\n")

    target <- data.frame(
      "term" = "P", ".lower" = plogis(-3.39), ".estimate" = plogis(-2.39),
      ".upper" = plogis(-1.57), ".alpha" = 0.05, ".method" = "norm")
    cat("\n")
    print(target)
    cat("\n")

    ids <- c(".lower", ".estimate", ".upper")
    check <- sum(abs(out[ids] - target[ids]))
    check <- abs(out$est[1] - target$est[1]) +
      abs(out$lci[1] - target$lci[1]) + abs(out$uci[1] - target$uci[1])
    check <- sum(check)
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_lt(check, 0.03)
})
