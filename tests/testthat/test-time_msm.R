test_that("time_msm", {
  data("cogdat", package = "fciR")
  out <- time_msm(cogdat, outcome.name = "Y",
                  exposure.names = c("A1", "A2"),
                  confound.names = "H2") |>
    mutate(estimate = round(estimate, 3))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  the_results <- c(
    "beta 0" = 0.261,
    "beta 1" = -0.008,
    "beta 2" = -0.060,
    "beta 3" = 0.208,
    "beta 1+3" = 0.199,
    "beta 2+3" = 0.148,
    "beta 1+2+3" = 0.401
  )
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")


  expect_identical(out, target)
})
