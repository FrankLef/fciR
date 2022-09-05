#' Monte Carlo Simulation of Doubly Robust Standardization
#'
#' @param ss Integer(). Number of covariates.
#' @param nrep Number of Monte Carlo repetitions.
#' @param width Width of interval. e.g. 0.95 will give interval c(0.025, 0.975).
#' Default is 0.95.
#'
#' @seealso standdr_sim standdr_est
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by ungroup summarize n
#' @importFrom stats sd quantile weighted.mean
#'
#' @source Section 6.3.1
#'
#' @return Dataframe of results.
#' @export
mc_standdr <- function(ss = c(40, 100), nrep = 1000, width = 0.95) {
  stopifnot(all(ss >= 1), nrep >= 1, width > 0, width < 1)

  # We use alpha = 0.15 to match results with the books
  mc_standdr_func <- function(n = 3000, ss = 100, alpha = 0.13, beta = 20,
                              probH = 0.05, seed = NULL) {

    # first we simulate the data
    dat <- standdr_sim(n = n, ss = ss, alpha = alpha, beta = beta,
                       probH = probH, seed = seed)$data
    # then output the estimates in a list
    standdr_est(Y = dat$Y, `T` = dat$`T`, H = dat$H)
  }

  params <- list("ss" = ss)
  mc.out <- MonteCarlo::MonteCarlo(func = mc_standdr_func,
                                   nrep = nrep, param_list = params)

  # output results in a dataframe
  out <- suppressWarnings(MonteCarlo::MakeFrame(mc.out))
  out %>%
    pivot_longer(cols = -ss, names_to = "estimator", values_to = "value") %>%
    group_by(ss, .data[["estimator"]]) %>%
    summarize(n = dplyr::n(),
              mean = mean(.data[["value"]]),
              sd = sd(.data[["value"]]),
              lower = quantile(.data[["value"]], probs = (1 - width) / 2),
              upper = quantile(.data[["value"]], probs = 1 - (1 - width) / 2)) %>%
    ungroup()
}

#' Data Simulation for Doubly Robust Standardization
#'
#' @param n Number of individuals/observations.
#' @param ss Number of covariates.
#' @param alpha coefficient used to compute the distribution of \code{`T`}.
#' @param beta coefficient used to compute the distribution of \code{`T`}.
#' @param probH probability of H.
#' @param seed Seed value. default is \code{NULL}.
#'
#' @importFrom stats rbinom
#'
#' @return List with a dataframe of Y, T and H and summary statitics.
#' @export
standdr_sim <- function(n = 3000, ss = 100, alpha = 0.13, beta = 20,
                        probH = 0.05, seed = NULL) {
  set.seed(seed)

  # matrix of independent Bernoulli vector with prob = 0.05
  # "The columns of H were independent indicator variables each
  #  with probability 0.05"
  H <- cbind(replicate(n = ss, rbinom(n = n, size = 1, prob = probH)))

  # let the treatment depend on a function of H
  # "We simulated T  as indicator variables with probabilities that varied as
  # a linear function  of H such that approximately 600 individuals had T=1"
  sumH <- apply(H, MARGIN = 1, FUN = sum) * beta / ss
  probT <- alpha * sumH + probH * rnorm(n = n, mean = 1, sd = 0.1)
  # validate the positivity assumption
  stopifnot(probT > 0, probT < 1)

  `T` <- rbinom(n = n, size = 1, prob = probT)


  # generate the outcome depend on T and H
  # "We simulated Y as a function T ans sumH such hat approximatey 35
  # individuals had Y = 1"
  probY <- 0.01 * `T` + 0.01 * sumH
  # positivity assumption is not required for the outcome
  # see intro to chapter 6 p. 99
  stopifnot(probY >= 0, probY <= 1)

  Y <- rbinom(n = n, size = 1, prob = probY)

  # put the data in a data.frame
  df <- data.frame("Y" = Y, "T" = `T`, "H" = H)

  # output results in a list
  list(
    "stats" = list("sumH" = standdr_stats(sumH),
                   "probT" = standdr_stats(probT),
                   "T" = standdr_stats(`T`),
                   "probY" = standdr_stats(probY),
                   "Y" = standdr_stats(Y)),
    "data" = list("Y" = Y, "T" = `T`, "H" = H)
  )
}


#' Estimates from Doubly Robust Standardization Simulation
#'
#' @param Y Vector of outcomes
#' @param T Vector of treatments
#' @param H Matrix of covariates
#'
#' @importFrom stats weighted.mean
#'
#' @return List of estimates
#' @export
standdr_est <- function(Y, `T`, H) {

  # fit the exposure model
  e <- fitted(lm(`T` ~ H))

  # refit the exposure model using an incorrect logistic model
  e2 <- predict(glm(`T` ~ H, family = "binomial"), type = "response")

  # compute the weights
  w0 <- (1 - `T`) / (1 - e)
  w1 <- `T` / e
  w02 <- (1 - `T`) / (1 - e2)
  w12 <- T / e2

  # fit an overspecified (saturated) outcome model
  mod.out <- lm(Y ~ `T` * H)

  # Estimate the expected potential outcomes using the various methods
  dat <- data.frame("Y" = Y, "T" = `T`)
  dat0 <- dat
  dat0$`T` <- 0
  dat1 <- dat
  dat1$`T` <- 1

  # the predicted data
  preds0 <- predict(mod.out, newdata = dat0)
  preds1 <- predict(mod.out, newdata = dat1)

  # calculate the estimates
  EYT0 <- mean(Y * (1 - `T`))
  EYT1 <- mean(Y * `T`)
  EY0exp <- weighted.mean(Y, w = w0)
  EY1exp <- weighted.mean(Y, w = w1)
  EY0exp2 <- weighted.mean(Y, w = w02)
  EY1exp2 <- weighted.mean(Y, w = w12)
  EY0out <- mean(preds0)
  EY1out <- mean(preds1)
  EY0dr <- mean(w0 * Y + preds0 * (`T` - e) / (1 - e))
  EY1dr <- mean(w1 * Y - preds1 * (`T` - e) / e)

  list("EYT0" = EYT0,
       "EYT1" = EYT1,
       "EY0exp" = EY0exp,
       "EY1exp" = EY1exp,
       "EY0exp2" = EY0exp2,
       "EY1exp2" = EY1exp2,
       "EY0out" = EY0out,
       "EY1out" = EY1out,
       "EY0dr" = EY0dr,
       "EY1dr" = EY1dr)
}

#' Compute Statistics from \code{standdr_sim}.
#'
#' @param x Vector of numeric values.
#'
#' @return list of statistics: \code{sun(x), mean(x), min(x), max(x)}.
#' @export
standdr_stats <- function(x) {
  list("sum" = sum(x), "mean" = mean(x), "min" = min(x), "max" = max(x))
}
