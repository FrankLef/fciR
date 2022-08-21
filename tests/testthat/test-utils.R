test_that("audit_formula", {

  # outcome.name must be in data
  a_formula <- wrong ~ Species * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species",
                             extra.names = "Sepal.Width"),
               regexp = "Assertion on \'outcome.name\'")

  # independent variables must be in data
  a_formula <- Sepal.Length ~ wrong * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species",
                             extra.names = "Sepal.Width"),
               regexp = "Assertion on \'ind.vars\'")

  #  variable must be in formula
  a_formula <- Sepal.Length ~ Species * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "wrong",
                             extra.names = "Sepal.Width"),
               regexp = "Assertion on \'exposure.name\'")

  # must return a list with 3 named elements
  a_formula <- Sepal.Length ~ Species * Sepal.Width
  out <- audit_formula(iris, formula = a_formula,
                       exposure.name = "Species",
                       extra.names = "Sepal.Width")
  target <- list(outcome.name = "Sepal.Length",
                 exposure.name = "Species",
                 extra.names = "Sepal.Width")
  expect_identical(out, target)
})
