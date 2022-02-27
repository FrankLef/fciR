#' Create Mortality by Country dataset in long format
#'
#' Create Mortality by Country dataset in long format.
#'
#' Create Mortality by Country dataset for exposure modeling as described
#' in section 6.2 of chapter 6.
#'
#' @return Dataframe of mortality rates and exposure model
data_mortality_long <- function() {
  out <- data.frame(
    "H" = c(0, 0, 0, 0, 1, 1, 1, 1),
    "T" = c(0, 0, 1, 1, 0, 0, 1, 1),
    "Y" = c(0, 1, 0, 1, 0, 1, 0, 1),
    "n" = c(1297258493 - 2923480,
            2923480,
            282305227 - 756340,
            756340,
            133015479 - 7517520,
            7517520,
            48262955 - 2152660,
            2152660))
  # compute proportion who died
  out$p <- out$n / sum(out$n)
  stopifnot(sum(out$p) == 1)

  out
}

# mortality_long <- data_mortality_long()
# usethis::use_data(mortality_long, overwrite = TRUE)
