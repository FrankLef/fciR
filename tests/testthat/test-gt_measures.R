test_that("gt_measures", {

  data(fci_tbl_03_02)
  df <- fci_tbl_03_02
  # cat("\n")
  # print(df)
  # cat("\n")

  title <- "Table 3.2"
  subtitle = paste0("4 Association Measures Relating",
                    "<br>",
                    "<em>More than High SchoolEducation</em>
                    to <em>Voting for Trump</em>")

  out <- gt_measures(df, title = title, subtitle = subtitle)

  expect_s3_class(out, class = "gt_tbl")
})
