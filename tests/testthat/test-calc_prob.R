test_that("calc_prob", {
  out <- calc_prob(iris, var.name = "Sepal.Length", prob.name = "prob")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_identical(dim(out), c(35L, 2L))
  expect_equal(sum(out$prob), 1)

})

test_that("calc_prob_cond", {
  out <- calc_prob_cond(iris, condition.names = "Species",
                    var.name = "Sepal.Length", prob.name = "prob")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_identical(dim(out), c(57L, 3L))
})
