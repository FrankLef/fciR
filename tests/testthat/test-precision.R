test_that("precision_eff", {
  data("doublewhatifdat")
  out <- doublewhatifdat.prec1.fit <- precision_eff(
    doublewhatifdat, formula = VL1 ~ `T` + AD0,
    exposure.name = "T", precision.name = "AD0") |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 6)))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("None", "AD0"),
    estimate = c(-0.1473304, -0.1514711),
    std.err = c(0.03085432, 0.02617973)) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 6)))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

test_that("precision_stats", {
  data("doublewhatifdat")
  doublewhatifdat.models <- list(
    "Model VL0" = list(
      "formula" = VL1 ~ `T` + VL0,
      "vars" = c("exposure" = "T", "precision" = "VL0")),
    "Model AD0" = list(
      "formula" = VL1 ~ `T` + AD0,
      "vars" = c("exposure" = "T", "precision" = "AD0"))
  )
  out <- precision_stats(data = doublewhatifdat,
                         models = doublewhatifdat.models,
                         times = 500, seed = 409)  # chen prime

  expect_identical(names(out), c("stats", "models"))

  out <- out$stats |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 3)))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("None", "VL0", "AD0"),
    estimate = c(-0.1473304, -0.1459360, -0.1514711),
    std.err = c(0.03085432, 0.03026407, 0.02617973),
    .lower = c(-0.2116576, -0.2069009, -0.2116576),
    .upper = c(-0.09010154, -0.08695798, -0.09010154)) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 3)))

  expect_identical(out, target)
})
