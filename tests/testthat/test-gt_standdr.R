test_that("gt_standdr", {

  data(fci_tbl_06_13)
  df <- fci_tbl_06_13

  df <- df %>% select(ss, estimator, description, mean, sd, pval) %>%
    mutate(ss = paste("ss", ss, sep = "=")) %>%
    pivot_longer(cols = c("mean", "sd", "pval"), names_to = "stats",
                 values_to = "value") %>%
    mutate(value = ifelse(stats == "pval", round(value, 2), round(value, 4))) %>%
    unite(col = "heading", ss, stats, sep = "_") %>%
    pivot_wider(id_cols = c("estimator", "description"), names_from = "heading",
                values_from = "value")

  expect_identical(dim(df), c(10L, 8L))

  title <- "Table 6.13 and 6.14"
  subtitle <- paste0("Sampling Distribution from Simulation", "<br>",
                     "Investigating Small-Sample Robustness", "<br>",
                     "True E(Y(0))=0.01, True E(Y(1))=0.02")

  out <- gt_standdr(df, title = title, subtitle = subtitle)
  expect_s3_class(out, "gt_tbl")
})
