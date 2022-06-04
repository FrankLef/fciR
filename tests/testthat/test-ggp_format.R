test_that("ggp_format: input check", {
  expect_error(ggp_format(df, type = "wrong"))
})

test_that("ggp_format: none", {
  df <- data.frame(
    name = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    conf = 0.95,
    est = c(0.375, 0.289, -0.086, 0.770, 0.879, 0.677),
    lci = c(0.262, 0.207, -0.206, 0.538, 0.729, 0.394),
    uci = c(0.485, 0.0371, 0.036, 1.109, 1.069, 1.179)
  )
  out <- ggp_format(df, type = "none")
  expect_identical(out, df)
})


test_that("ggp_format: measures_tbl", {
  df <- data.frame(
    name = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    conf = 0.95,
    est = c(0.375, 0.289, -0.086, 0.770, 0.879, 0.677),
    lci = c(0.262, 0.207, -0.206, 0.538, 0.729, 0.394),
    uci = c(0.485, 0.0371, 0.036, 1.109, 1.069, 1.179)
  )
  out <- ggp_format(df, type = "measures_tbl")
  expect_type(out, type = "list")
})
