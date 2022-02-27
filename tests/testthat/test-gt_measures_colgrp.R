test_that("gt_measures_colgrp", {

  data(fci_tbl_04_02)
  df <- fci_tbl_04_02

  out <- gt_measures_colgrp(df, var_grp = "group",
                            title = "Table 4.2 RECOVERY Trial",
                            subtitle = "Effect-measure Modification")
  expect_s3_class(out, class = "gt_tbl")
})
