test_that("ggp_measures_modif", {
  data(fci_tbl_04_02)
  df <- fci_tbl_04_02

  out <- ggp_measures_modif(df, title = "Change in effect measures",
                     subtitle = "Effect-Measure Modification and Statistical Interaction")

  expect_s3_class(out, class = "ggplot")
})
