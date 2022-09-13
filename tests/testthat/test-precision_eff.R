test_that("precision_eff", {
  data("doublewhatifdat")
  out <- doublewhatifdat.prec1.fit <- precision_eff(
    doublewhatifdat, formula = VL1 ~ `T` + AD0,
    exposure.name = "T", precision.name = "AD0") |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 6))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("None", "AD0"),
    estimate = c(-0.1473304, -0.1514711),
    std.err = c(0.03085432, 0.02617973)) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 6))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
