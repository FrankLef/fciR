#' \code{doublewhatifsim} script rewritten
#'
#' \code{doublewhatifsim} script rewritten.
#'
#' Simulate the What-If study data.
#'
#' @param n Nb of observations.
#' @param seed Integer, the seed used for random numbers.
#'
#' @importFrom stats rbinom
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' sim_doublewhatif()
#' }
#'
sim_doublewhatif <- function(n = 1000, seed = 444) {

  set.seed(seed)

  # variables each with probability 0.5
  U <- rbinom(n, size = 1, prob = 0.5)
  # probability of AD0 depends on U
  AD0prob <- 0.2 + 0.6 * U
  # generate independent bernoulli variables with varying probabilities
  AD0 <- rbinom(n, size = 1, prob = AD0prob)
  VL0prob <- 0.8 - 0.4 * AD0
  VL0 <- rbinom(n, size = 1, prob = VL0prob)
  `T` <- rbinom(n, size = 1, prob = 0.5)
  Aprob <- 0.05 + `T` * U * 0.8
  A <- rbinom(n, size = 1, prob = Aprob)
  AD1prob <- 0.1 + 0.8 * A
  AD1 <- rbinom(n, size = 1, prob = AD1prob)
  VL1prob <- VL0prob + 0.1 - 0.45 * AD1
  VL1 <- rbinom(n, size =1 , prob = VL1prob)

  data.frame(
    "AD0" = AD0,
    "VL0" = VL0,
    "U" = U,
    "T" = `T`,
    "A" = A,
    "AD1" = AD1,
    "VL1" = VL1
  )
}

# to create dataset
# doublewhatifdat <- sim_doublewhatif()
# usethis::use_data(doublewhatifdat, overwrite = TRUE)

