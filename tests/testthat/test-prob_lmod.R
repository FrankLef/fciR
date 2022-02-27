test_that("prob_lmod", {
  data("whatifdat")
  data <- whatifdat

  out <- prob_lmod(data, formula = Y ~ `T` + A + H,
                  cond = Y ~ `T` + A + H,
                  R = 500, conf = 0.95)
  expect_true(abs(out["est"] - 0.60596) < 0.03)
  expect_true(abs(out["lci"] - 0.41638) < 0.03)
  expect_true(abs(out["uci"] - 0.76823) < 0.03)

})
