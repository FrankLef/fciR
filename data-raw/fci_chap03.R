#' Create data for table 3.2
#'
#' @return Dataframe
fci_tbl_03_02 <- function() {
  data <- data.frame(
    term = c("P0", "P1", "RD", "logRR", "RR*", "OR"),
    .lower = c(0.21, 0.241, 0, 1.004, 1, 1.004),
    .estimate = c(0.233, 0.271, 0.038, 1.162, 1.052, 1.222),
    .upper = c(0.256, 0.301, 0.075, 1.347, 1.106, 1.49),
    .alpha = 0.05,
    .method = "norm"
  )
}

# fci_tbl_03_02 <- fci_tbl_03_02()
# usethis::use_data(fci_tbl_03_02, overwrite = TRUE)
