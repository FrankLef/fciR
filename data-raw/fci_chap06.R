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


#' Create data for table 6.13 and 6.14
#'
#' @return Dataframe
fci_tbl_06_13 <- function() {
  the_estimators <- c("EYT0" = "Unadjusted", "EYT1" = "Unadjusted",
                      "EY0exp" = "Linear Exposure", "EY1exp" = "Linear Exposure",
                      "EY0exp2" = "Logistic Exposure", "EY1exp2" = "Logistic Exposure",
                      "EY0out" = "Overspecified Outcome", "EY1out" = "Overspecified Outcome",
                      "EY0dr" = "Doubly Robust", "EY1dr" = "Doubly Robust")
  bb40 <- data.frame(
    ss = 40,
    estimator = names(the_estimators),
    description = the_estimators,
    mean = c(0.0076, 0.0042, 0.01, 0.0195, 0.0101, 0.0204,
             0.01, 0.02, 0.01, 0.0197),
    sd = c(0.0015, 0.0012, 0.0021, 0.0127, 0.0021, 0.0064,
           0.0021, 0.0066, 0.0021, 0.0106),
    pval = c(0, 0, 0.92, 0.19, 0.42, 0.07, 0.79, 0.84, 0.82, 0.37)
  )

  bb100 <- data.frame(
    ss = 100,
    estimator = names(the_estimators),
    description = the_estimators,
    mean = c(0.0079, 0.0038, 0.01, 0.0196, 0.01, 0.02, 0.01, 0.02, 0.01, 0.029),
    sd = c(0.0016, 0.0012, 0.002, 0.0562, 0.002, 0.0068,
           0.002, 0.0069, 0.002, 0.1891),
    pval = c(0, 0, 0.61, 0.81, 0.73, 0.96, 0.74, 0.74, 0.72, 0.14)
  )
  data <- as.data.frame(rbind(bb40, bb100))
}

fci_tbl_06_13 <- fci_tbl_06_13()
usethis::use_data(fci_tbl_06_13, overwrite = TRUE)
