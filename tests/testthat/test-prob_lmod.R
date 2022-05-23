test_that("prob_lmod", {
  data("whatifdat")
  data <- whatifdat

  out <- prob_lmod(data,
                   formula = Y ~ `T` + A + H,
                   condition.names = c("T", "A"))
  out <- round(out, 6)
  # cat("\n")
  # print(out)
  # cat("\n")

  target <- c("logitP" = round(qlogis(0.09001343), 6))
  # cat("\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})


test_that("prob_lmod_td", {
  data("whatifdat")
  data <- whatifdat

  out <- prob_lmod_td(data, formula = Y ~ `T` + A + H,
                      condition.names = c("T", "A"))
  # cat("\n")
  # print(out)
  # cat("\n")
  out <- round(c("logitP" = out$estimate), 6)

  target <- c("logitP" = round(qlogis(0.09001343), 6))
  # cat("\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
