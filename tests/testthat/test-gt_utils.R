test_that("gt_basic", {
  data <- data.frame(
   name = c("P0", "P1", "RD", "logRR", "RR*", "OR"),
   est = c(0.233, 0.271, 0.038, 1.162, 1.052, 1.222),
   conf = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95),
   lci = c(0.21, 0.241, 0, 1.004, 1, 1.004),
   uci = c(0.256, 0.301, 0.075, 1.347, 1.106, 1.49)
  )
  title <- "Table 3.2"
  subtitle = paste0("4 Association Measures Relating",
                    "<br>",
                    "<em>More than High SchoolEducation</em>
                    to <em>Voting for Trump</em>")

  out <- data %>% gt::gt() %>% gt_basic(title = title, subtitle = subtitle)

  expect_s3_class(out, class = "gt_tbl")
})
