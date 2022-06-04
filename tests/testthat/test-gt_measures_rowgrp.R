test_that("gt_measures_rowgrp", {
  data(fci_tbl_07_02)
  df <- fci_tbl_07_02
  # cat("\n")
  # print(df)
  # cat("\n")

  subtitle <- paste0("Double What-If Study", "<br>",
                     "Difference-in-Differences Estimation of the ATT")
  out <- gt_measures_rowgrp(df, rowgroup = "term", rowname = "model",
                            title = "Table 7.2",
                            subtitle = subtitle)
  expect_s3_class(out, class = "gt_tbl")
})
