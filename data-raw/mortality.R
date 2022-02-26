#' Create dataset Mortality Rate by Country
#'
#' Create dataset Mortality Rate by Country.
#'
#' Create dataset Mortality Rate by Country as shown in section 1.2.1.
#'
#' @return Dataframe of mortability rates
data_mortality <- function() {
  out <- data.frame(
    "T" = c(TRUE, TRUE, FALSE, FALSE),
    "H" = c(FALSE, TRUE, FALSE, TRUE),
    "deaths" = c(756340, 2152660, 2923480, 7517520),
    "population" = c(282305227, 48262955, 1297258493, 133015479))
  out$Y <- out$deaths / out$population
  # verify with book
  check <- c(0.002679, 0.0446, 0.002254, 0.05652)
  stopifnot(sum(abs(out$Y - check)) < 0.0001)
  out
}

mortality <- data_mortality()
usethis::use_data(mortality, overwrite = TRUE)
