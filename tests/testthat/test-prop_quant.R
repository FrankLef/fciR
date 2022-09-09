test_that("prop_quant", {
  data("gss")
  gssrcc <- gss[, c("trump", "gthsedu", "magthsedu", "white", "female", "gt65")]
  gssrcc <- gssrcc[complete.cases(gssrcc), ]

  out <- prop_quant(gssrcc, outcome.name = "trump", exposure.name = "gthsedu",
                    confound.names = c("magthsedu", "white", "female", "gt65"),
                    probs = 0:4/4) |>
    mutate(estimate = round(estimate, 6))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame("term" = "meanRD",
                       "estimate" = round(0.03045919, 6),
                       "std.err" = NA_real_)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
