test_that("gt_standdr", {
  the_estimators <- c("EYT0" = "Unadjusted", "EYT1" = "Unadjusted",
                      "EY0exp" = "Linear Exposure", "EY1exp" = "Linear Exposure",
                      "EY0exp2" = "Logistic Exposure", "EY1exp2" = "Logistic Exposure",
                      "EY0out" = "Overspecified Outcome", "EY1out" = "Overspecified Outcome",
                      "EY0dr" = "Doubly Robust", "EY1dr" = "Doubly Robust")
  bb40 <- data.frame(
    ss = 40,
    estimator = names(the_estimators),
    description = the_estimators,
    mean = c(0.0076, 0.0042, 0.01, 0.0195, 0.0101, 0.0204,
             0.01, 0.02, 0.01, 0.0197),
    sd = c(0.0015, 0.0012, 0.0021, 0.0127, 0.0021, 0.0064,
           0.0021, 0.0066, 0.0021, 0.0106),
    pval = c(0, 0, 0.92, 0.19, 0.42, 0.07, 0.79, 0.84, 0.82, 0.37)
  )

  bb100 <- data.frame(
    ss = 100,
    estimator = names(the_estimators),
    description = the_estimators,
    mean = c(0.0079, 0.0038, 0.01, 0.0196, 0.01, 0.02, 0.01, 0.02, 0.01, 0.029),
    sd = c(0.0016, 0.0012, 0.002, 0.0562, 0.002, 0.0068,
           0.002, 0.0069, 0.002, 0.1891),
    pval = c(0, 0, 0.61, 0.81, 0.73, 0.96, 0.74, 0.74, 0.72, 0.14)
  )
  data <- rbind(bb40, bb100)

  title <- "Table 6.13 and 6.14"
  subtitle <- paste0("Sampling Distribution from Simulation", "<br>",
                     "Investigating Small-Sample Robustness", "<br>",
                     "True E(Y(0))=0.01, True E(Y(1))=0.02")

  out <- gt_standdr(data, title = title, subtitle = subtitle)
  expect_s3_class(out, "gt_tbl")
})
