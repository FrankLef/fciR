test_that("ggp_measures_df", {
  df <- data.frame(
    term = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    .lower = c(0.262, 0.207, -0.206, 0.538, 0.729, 0.394),
    .estimate = c(0.375, 0.289, -0.086, 0.770, 0.879, 0.677),
    .upper = c(0.485, 0.0371, 0.036, 1.109, 1.069, 1.179),
    .alpha = 0.05
  )
  out <- ggp_measures_df(df)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c("RD", "RR", "RR*", "OR", "AF\U2020", "CP\U2020")

  expect_identical(dim(out), c(6L, 7L))
  expect_identical(out$term, target)
})


test_that("ggp_measures_df: error", {
  df <- data.frame(
    term = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    .lower = c(0.262, 0.207, -0.206, 0.538, 0.729, 0.394),
    .estimate = c(0.375, 0.289, -0.086, 0.770, 0.879, 0.677),
    .upper = c(0.485, 0.0371, 0.036, 1.109, 1.069, 1.179),
    .alpha = 0.05
  )
  df <- df[df$name != "OR", ]
  expect_error(ggp_measures_df(df), class = "ggp_measures_df_error")
})


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
