test_that("prob_uncond", {
  data("whatifdat")
  data <- whatifdat

  out <- meas_effect_uncond(data, Y ~ `T`, R = 50)
  expect_identical(dim(out), c(6L, 5L))
  expect_true(all(is.finite(data$est)))
  expect_true(all(is.finite(data$conf)))
  expect_true(all(is.finite(data$lci)))
  expect_true(all(is.finite(data$uci)))
})