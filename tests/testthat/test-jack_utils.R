test_that("jack_ci", {
  df <- mtcars
  alpha <- 0.05

  out <- jack_ci(df$mpg)
  out <- round(out, 4)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c(".lower" = -35.9092, ".estimate" = 20.0906, ".upper" = 76.0905)

  expect_identical(out, target)
})

test_that("jack_run", {
  ids <- c("RD" = 2, "logRR" = 5, "logOR" = 8)

  is_skip <- TRUE
  if (!is_skip) {
    data(doublewhatifdat)
    out <- jack_run(doublewhatifdat, func = fciR::instr_linear, alpha = 0.05,
                    formula = VL1 ~ A + `T`, exposure.name = "A")
    out <- out[out$term %in% names(ids), ]
    # cat("\n", "out", "\n")
    # print(out)
    # cat("\n")


    data(fci_tbl_09_01a)
    target <- fci_tbl_09_01a
    target <- target[ids, ]
    # cat("\n", "target", "\n")
    # print(target)
    # cat("\n")

    check <- sum(abs(out$.estimate - target$.estimate))
  }
  skip_if(is_skip, message = "Skip to save time.")
  expect_lt(check, 1e-4)
})


test_that("jack_est", {
  ids <- c("RD" = 2, "logRR" = 5, "logOR" = 8)

  is_skip <- TRUE
  if (!is_skip) {
    data(doublewhatifdat)
    out <- jack_est(doublewhatifdat, func = fciR::instr_linear, alpha = 0.05,
                    formula = VL1 ~ A + `T`, exposure.name = "A")
    out <- out[out$term %in% names(ids), ]
    # cat("\n", "out", "\n")
    # print(out)
    # cat("\n")


    data(fci_tbl_09_01a)
    target <- fci_tbl_09_01a
    target <- target[ids, ]
    # cat("\n", "target", "\n")
    # print(target)
    # cat("\n")

    check <- sum(abs(out$.estimate - target$.estimate))
  }
  skip_if(is_skip, message = "Skip to save time.")
  expect_lt(check, 1e-4)
})
