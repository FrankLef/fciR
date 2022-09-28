test_that("time_snmm", {
  data("cogdat", package = "fciR")
  out <- time_snmm(cogdat, outcome.name = "Y",
                  exposure.names = c("A1", "A2"),
                  confound.names = "H2") |>
    mutate(estimate = round(estimate, 4))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  the_results <- c(
    "B20" = -0.2927,
    "B20+B22" = 0.4792,
    "B20+B21" = 0.3929,
    "B20+B21+B22+B23" = -0.1353,
    "B1" = -0.0081)
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")


  expect_identical(out, target)
})
