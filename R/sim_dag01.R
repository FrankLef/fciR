#' Simulate DAG # 1. Table 5.1.
#'
#' @param n Simulation size.
#' @param seed Integer,s seed.
#'
#' @importFrom stats rbinom
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' sim_dag01()
#' }
sim_dag01 <- function(n = 1000, seed = 111) {
  set.seed(111)
  probY0 <- 0.42
  probY1 <- 0.62
  Y0 <- rbinom(n, size = 1, prob = probY0)
  Y1 <- rbinom(n, size = 1, prob = probY1)
  probA <- (1 - Y0) * (1 - Y1) * 0.6307 +
    (1 - Y0) * Y1 * 0.4867 +
    Y0 * (1- Y1) * 0.4699 +
    Y0 * Y1 * 0.4263
  A <- rbinom(n, size = 1, prob = probA)
  Y <- A * Y1 + (1 - A) * Y0
  data.frame(cbind(A, Y0, Y1, Y))
}
