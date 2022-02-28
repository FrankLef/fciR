#' Create data for table 6.1
#'
#' @return Dataframe
fci_tbl_06_01 <- function() {
  data.frame(
    name = c("EY0", "EY1", "RD", "RR"),
    est = c(0.375, 0.289, -0.086, 0.77),
    conf = rep(0.95, 4),
    lci = c(0.263, 0.206, -0.212, 0.528),
    uci = c(0.487, 0.372, 0.039, 1.12))
}

# fci_tbl_06_01 <- fci_tbl_06_01()
# usethis::use_data(fci_tbl_06_01, overwrite = TRUE)

#' Create data for table 6.4
#'
#' @return Dataframe
fci_tbl_06_04 <- function() {
  data.frame(
    name = c("EY0", "EY1", "RD", "RR"),
    est = c(0.361, 0.276, -0.085, 0.765),
    conf = rep(0.95, 4),
    lci = c(0.247, 0.192, -0.207, 0.52),
    uci = c(0.476, 0.36, 0.037, 1.12))
}

# fci_tbl_06_04 <- fci_tbl_06_04()
# usethis::use_data(fci_tbl_06_04, overwrite = TRUE)

#' Create data for table 6.7
#'
#' @return Dataframe
fci_tbl_06_07 <- function() {
  data.frame(
    name = c("EY0", "EY1", "RD", "RR"),
    est = c(0.360, 0.300, -0.061, 0.831),
    conf = rep(0.95, 4),
    lci = c(0.249, 0.216, -0.188, 0.564),
    uci = c(0.472, 0.384, 0.067, 1.23))
}

# fci_tbl_06_07 <- fci_tbl_06_07()
# usethis::use_data(fci_tbl_06_07, overwrite = TRUE)

#' Create data for table 6.9
#'
#' @return Dataframe
fci_tbl_06_09 <- function() {
  data.frame(
    name = c("EY0", "EY1", "RD", "RR"),
    est = c(0.360, 0.300, -0.061, 0.834),
    conf = rep(0.95, 4),
    lci = c(0.249, 0.220, -0.188, 0.565),
    uci = c(0.471, 0.380, 0.069, 1.23))
}

# fci_tbl_06_09 <- fci_tbl_06_09()
# usethis::use_data(fci_tbl_06_09, overwrite = TRUE)
