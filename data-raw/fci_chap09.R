#' Create data for table 9.1
#'
#' @return Dataframe
fci_tbl_09_01 <- function() {
  data.frame(
    method = c(
      "CACE",
      "Linear", "Loglinear", "Logistic",
      "Linear", "Loglinear", "Logistic",
      "Linear", "Loglinear", "Logistic"),
    name = c(
      "E(Y(1)-Y(0)|C=1)",
      "E(Y-Y(0)|A=1)",
      "E(Y-Y(0)|A=1)",
      "E(Y-Y(0)|A=1)",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))"),
    Truth = c(NA_real_,
              -0.360, -0.360, -0.360,
              -1.033, -1.033, -1.033,
              -1.630, -1.630, -1.630),
    est = c(-0.409,
            -0.409, -0.483, -0.413,
            -1.020, -1.129, -1.025,
            -1.780, -2.116, -1.794),
    conf = rep(0.95, 10),
    lci = c(-0.564,
            -0.564, -0.746, -0.560,
            -1.340, -1.564, -1.339,
            -2.478, -3.409, -2.464),
    uci = c(-0.254,
            -0.254, -0.219, -0.265,
            -0.700, -0.693, -0.712,
            -1.081, -0.822, -1.124))
}

# fci_tbl_09_01 <- fci_tbl_09_01()
# usethis::use_data(fci_tbl_09_01, overwrite = TRUE)

#' Create data for table 9.1 with qt() instead of 1.96 for ci.
#'
#' @return Dataframe
fci_tbl_09_01a <- function() {
  data.frame(
    method = c(
      "CACE",
      "Linear", "Loglinear", "Logistic",
      "Linear", "Loglinear", "Logistic",
      "Linear", "Loglinear", "Logistic"),
    name = c(
      "E(Y(1)-Y(0)|C=1)",
      "E(Y-Y(0)|A=1)",
      "E(Y-Y(0)|A=1)",
      "E(Y-Y(0)|A=1)",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "log(E(Y|A=1))-log(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))",
      "logit(E(Y|A=1))-logit(E(Y(0)|A=1))"),
    Truth = c(NA_real_,
              -0.360, -0.360, -0.360,
              -1.033, -1.033, -1.033,
              -1.630, -1.630, -1.630),
    est = c(-0.409,
            -0.4093, -0.483, -0.413,
            -1.0201, -1.129, -1.025,
            -1.7795, -2.116, -1.794),
    conf = rep(0.95, 10),
    lci = c(-0.564,
            -0.5396, -0.746, -0.560,
            -1.2885, -1.564, -1.339,
            -2.3660, -3.409, -2.464),
    uci = c(-0.254,
            -0.2789, -0.219, -0.265,
            -0.7514, -0.693, -0.712,
            -1.1931, -0.822, -1.124))
}

# fci_tbl_09_01a <- fci_tbl_09_01a()
# usethis::use_data(fci_tbl_09_01a, overwrite = TRUE)
