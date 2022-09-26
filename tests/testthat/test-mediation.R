test_that("mediation_np", {
  data("doublewhatifdat", package = "fciR")
  out <- mediation_np(doublewhatifdat,
                      formula = VL1 ~ `T` + A + AD0,
                      exposure.name = "T",
                      mediator.name = "A",
                      confound.names = c("AD0")) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 5))
  row.names(out) <- seq_len(nrow(out))

  the_results <- c(
    "TE" = -0.15147635,
    "CDE(0)" = -0.04240090,
    "CDE(1)" = 0.04705889,
    "NDE" = -0.03552486,
    "NIE" = -0.11595150,
    "PE(0)" = 0.72008238,
    "PE(1)" = 1.31066825,
    "PM" = 0.76547589)
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 5))

  expect_identical(out, target)
})


test_that("mediation", {
  data("doublewhatifdat", package = "fciR")
  out <- mediation(doublewhatifdat,
                      formula = VL1 ~ `T` + A + AD0,
                      exposure.name = "T",
                      mediator.name = "A",
                      confound.names = c("AD0")) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 5))
  row.names(out) <- seq_len(nrow(out))

  the_results <- c(
    "TE" = -0.15147635,
    "CDE(0)" = -0.04240090,
    "CDE(1)" = 0.04705889,
    "NDE" = -0.03552486,
    "NIE" = -0.11595150,
    "PE(0)" = 0.72008238,
    "PE(1)" = 1.31066825,
    "PM" = 0.76547589)
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 5))

  expect_identical(out, target)
})
