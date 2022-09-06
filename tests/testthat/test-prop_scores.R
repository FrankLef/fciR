test_that("prop_scores", {
  data("gss")
  df <- gss
  prop <- prop_scores(df, formula = gthsedu ~ magthsedu + white+ female + gt65)
  out <- broom::tidy(prop$fit)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_length(prop, 2)

  target <- c(-1.20176, 1.29855, 0.50066, 0.19367, 0.00903)
  expect_lt(sum(abs(out$estimate - target)), 0.0001)
})
