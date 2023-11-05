test_that("mediation_calc", {
  out <- mediation_calc(NDE = -0.03552486,
                        NIE = -0.11595150,
                        CDE0 = -0.04240090,
                        CDE1 = 0.04705889) |>
    mutate(across(.cols = where(is.numeric), \(x) round(x, digits = 6)))


  the_results <- c("TE" = -0.15147635,
                   "PM" = 0.76547589,
                   "PE(0)" = 0.72008238,
                   "PE(1)" = 1.31066825)
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_) |>
    mutate(across(.cols = where(is.numeric), .fns = \(x) round(x, digits = 6)))

  expect_identical(out, target)
})


test_that("mediation_calc: error when TE = 0", {
  # error when TE = 0, i.e. NDE + NIE = 0
  expect_error(mediation_calc(NDE = -0.1,
                              NIE = 0.1,
                              CDE0 = -0.04240090,
                              CDE1 = 0.04705889),
               regexp = "Total effect must not be zero[.]")
})
