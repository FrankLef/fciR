test_that("formula2vars", {
  out <- formula2vars(Y ~ `T` + H + `T` * H)

  target <- list("y" = "Y", "t" = "T", "ind" = c("T", "H"), "x0" = "(Intercept)")
  target <- list("y" = "Y", "t" = "T", "h" = "H", "ht" = c("H", "T:H"),
                 "ind" = c("T", "H", "T:H"), "x0" = "(Intercept)")
  expect_identical(out, target)
})

test_that("formula2vars: minimal", {
  out <- formula2vars(Y ~ `T`)

  target <- list("y" = "Y", "t" = "T", "ind" = "T", "x0" = "(Intercept)")
  expect_identical(out, target)
})
