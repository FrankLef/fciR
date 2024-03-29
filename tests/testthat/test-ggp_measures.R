test_that("ggp_measures", {
  df <- data.frame(
    term = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    .lower = c(0.262, 0.207, -0.206, 0.538, 0.729, 0.394),
    .estimate = c(0.375, 0.289, -0.086, 0.770, 0.879, 0.677),
    .upper = c(0.485, 0.0371, 0.036, 1.109, 1.069, 1.179),
    .alpha = 0.05
  )
  out <- ggp_measures(df, title = "Table 6.1 What-If Study",
                      subtitle = "Standardized Estimates")
  expect_s3_class(out, class = "ggplot")
})
