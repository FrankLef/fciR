test_that("boot_est: Bootstrapping with base R", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_est(data = df, func = prob_lmod, R = 500, conf = 0.95,
                    inv = "expit", evars = "logit",
                    outcome.name = "Y",
                    input.names = c("T", "A", "H"),
                    condition.names = c("T", "A"))
    # cat("\n")
    # print(out)
    # str(out)
    # cat("\n")

    target <- data.frame(
      "name" = "P", "est" = 0.0900, conf = 0.95, "lci" = 0.0419, "uci" = 0.2086)

    ids <- c("est", "lci", "uci")
    check <- sum(abs(out[ids] - target[ids]))
    check <- abs(out$est[1] - target$est[1]) +
      abs(out$lci[1] - target$lci[1]) + abs(out$uci[1] - target$uci[1])
    check <- sum(check)
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_lt(check, 0.03)
})


test_that("boot_run_td: Bootstrapping with tidyverse", {
  data("whatifdat")
  df <- whatifdat

  is_skip <- TRUE
  if (!is_skip) {
    out <- boot_run_td(data = df, func = prob_lmod_td, times = 1000, alpha = 0.05,
                       outcome.name = "Y",
                       input.names = c("T", "A", "H"),
                       condition.names = c("T", "A")) |>
      suppressWarnings()
    cat("\n")
    print(out)
    cat("\n")

    target <- data.frame(
      term = "logitP", .lower = -3.39, .estimate = -2.39,
      .upper = -1.57, .alpha = 0.05, .method = "percentile")
    cat("\n")
    print(target)
    cat("\n")

    check <- abs(out$.lower[1] - target$.lower[1]) +
      abs(out$.estimate[1] - target$.estimate[1]) +
      abs(out$.upper[1] - target$.upper[1])
    check <- sum(check)
  }

  skip_if(is_skip, "Skip to save time.")
  expect_identical(names(out), names(target))
  expect_lt(check, 0.1)
})
