#' Simulate a sampling distribution
#'
#' Simulate a sampling distribution.
#'
#' Simulate a sampling distribution using base R. Section 2.4.
#'
#' @param nsim Nb of replicates.
#' @param n Nb of observations.
#'
#' @importFrom stats rnorm var
#'
#' @return List with percentage of bad and good.
#'
#' @examples
#' sim_intervals(nsim = 10, n = 10)
#' @export
sim_intervals <- function(nsim = 1e4, n = 1e4) {
  # get the statistics from the 2 samplings
  out <- replicate(n = nsim, expr = {
    y <- rnorm(n)
    x <- rnorm(n)
    muhaty <- mean(y)  # mean of the y sample
    sehaty <- sqrt(var(y) / n)  # standard error of the y sample
    lci <- muhaty - 1.96 * sehaty  # lower bound of the y sample
    uci <- muhaty + 1.96 * sehaty  # upper bound of the y sample
    muhatx <- mean(x)  # mean of the x sample
    # is mean(x) within the interval from y?
    bad <- (muhatx >= lci) & (muhatx <= uci)
    # is the true mean of 0 within the interval from y?
    good <- (0 >= lci) & (0 <= uci)
    # return the results
    c("bad" = bad, "good" = good)
  })
  # convert to data.frame
  out <- data.frame(t(out))
  pbad <- sum(out$bad) / nrow(out)  # proportion in bad interval
  pgood <- sum(out$good) / nrow(out)  # proportion in good interval
  # return results in a list
  list("bad" = pbad, "good" = pgood)
}

#' @rdname sim_intervals
#' @examples
#' \dontrun{
#' sim()
#' }
#' @export
sim <- sim_intervals
