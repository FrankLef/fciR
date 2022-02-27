test_that("boot_cond", {
  data("whatifdat")
  data <- whatifdat

  out <- boot_cond(data, formula = Y ~ `T` + A + H,
                   cond0 = Y ~ A + H,
                   cond1 = Y ~ `T` + A + H,
                   R = 50)

  expect_identical(dim(out), c(6L, 5L))
  expect_true(all(is.finite(data$est)))
  expect_true(all(is.finite(data$conf)))
  expect_true(all(is.finite(data$lci)))
  expect_true(all(is.finite(data$uci)))
})
