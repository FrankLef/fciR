test_that("gt_measures_rowgrp", {
  data <- data.frame(
    method = c("Linear","Loglinear", "Logistic",
               "Linear","Loglinear", "Logistic",
               "Linear","Loglinear", "Logistic",
               "Linear","Loglinear", "Logistic"),
    name = c(
      "E(Y(0)|A=1)", "E(Y(0)|A=1)", "E(Y(0)|A=1)",
      "RD", "RD", "RD",
      "RR", "RR", "RR",
      "OR", "OR", "OR"),
    Truth = c(0.559, 0.559, 0.559, -0.36, -0.36, -0.36,
              0.356, 0.356, 0.356, 0.196, 0.196, 0.196),
    est = c(0.586, 0.577, 0.592, -0.355, -0.346, -0.362,
            0.394, 0.400, 0.390, 0.212, 0.22, 0.206),
    lci = c(0.508, 0.498, 0.513, -0.441, -0.431, -0.447,
            0.309, 0.315, 0.306, 0.142, 0.149, 0.139),
    uci = c(0.664, 0.656, 0.671, -0.270, -0.262, -0.276,
            0.500, 0.508, 0.496, 0.315, 0.325, 0.307))

  out <- gt_measures_rowgrp(data, rowgroup = "name", rowname = "method",
                            conf = 0.95, title = "Table 7.2",
                            subtitle = "Double What-If Study<br>Difference-in-Differences Estimation of the ATT"
  )
  expect_s3_class(out, class = "gt_tbl")
})
