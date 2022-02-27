test_that("gt_probs", {
  data(fci_tbl_05_01)
  df <- fci_tbl_05_01 %>%
    dplyr::group_by(A, Y0, Y1, Y) %>%
    dplyr::count(name = "prob") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prob = prob / sum(prob))

  out <- gt_probs(df, title = "Table 5.1",
                  subtitle = "Simulation Probabilities for `sim1.r`")

  expect_s3_class(out, class = "gt_tbl")
})
