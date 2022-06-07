test_that("audit_formula", {

  # outcome.name must be in data
  a_formula <- wrong ~ Species * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species"))

  # independent variables must be in data
  a_formula <- Sepal.Length ~ wrong * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species"))

  # exposure variable must be in formula
  a_formula <- Sepal.Length ~ Species * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "wrong"))

  # nvars must be >= 1 ot NA
  a_formula <- Sepal.Length ~ Species * Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species",
                             nvars = 0))

  # the default nb extra variables must be >= 1
  a_formula <- Sepal.Length ~ Species
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species"))

  # the nb of extra variables must be equal to nvars when required
  # In this case there is 1 extra but 2 are required
  a_formula <- Sepal.Length ~ Species + Sepal.Width
  expect_error(audit_formula(iris, formula = a_formula,
                             exposure.name = "Species",
                             nvars = 2))

  # must return a list with 3 named elements
  a_formula <- Sepal.Length ~ Species * Sepal.Width
  out <- audit_formula(iris, formula = a_formula, exposure.name = "Species")
  target <- list(outcome.name = "Sepal.Length",
                 exposure.name = "Species",
                 extra.names = "Sepal.Width")
  expect_identical(out, target)
})
