test_that("gt_measures_rowgrp", {
  data(fci_tbl_07_02)
  df <- fci_tbl_07_02

  subtitle <- paste0("Double What-If Study", "<br>",
                     "Difference-in-Differences Estimation of the ATT")
  out <- gt_measures_rowgrp(df, rowgroup = "name", rowname = "method",
                            conf = 0.95, title = "Table 7.2",
                            subtitle = subtitle)
  expect_s3_class(out, class = "gt_tbl")
})
