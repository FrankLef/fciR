#' Compute estimates of the unconditional association measures
#'
#' Compute estimates of the unconditional association measures.
#'
#' Compute estimates of the unconditional association measures,
#' hence the u in \code{bootu()}, and their confidence intervals.
#' 1- We use Gaussian, Poisson and Binomial glm to solve the
#'    estimating equations of the measures, not their distributions
#' 2- The association measures' distributions (ci) are estimated by
#'    bootstrapping
#' Assumptions: We assume that (3.2) holds but not (3.1)
#' See p. 45 and 46 for more details.
#' @param data Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval
#'
#' @return Dataframe of summarized results
boot_uncond <- function(data, formula = Y ~ `T`, R = 1000, conf = 0.95) {

  # Extract variable names from the formula
  fvars <- formula2vars(formula)

  # estimate the conditional probabilities
  # and the four association measures
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # estimate the conditional probabilities
    coefs <- coef(glm(formula = formula, family = "gaussian", data = dat))
    p0 <- coefs[fvars$x0]
    p1 <- sum(coefs)
    # estimate the risk difference
    rd <- p1 - p0

    # use loglinear model to estimate the log relative risk
    coefs <- coef(glm(formula = formula, family = "poisson", data = dat))
    logrr <- coefs[fvars$ind]

    # prepare data to estimate the log other relative risk
    ystar <- 1 - dat[, fvars$y]
    xstar <- 1 - dat[, fvars$ind]

    # use loglinear model to estimate the log other relative risk
    coefs <- coef(glm(ystar ~ xstar, family = "poisson"))
    logrrstar <- coefs[2]

    # use logistic model to estimate the log of other risk
    coefs <- coef(glm(formula = formula, family = "binomial", data = dat))
    logor <- coefs[fvars$ind]

    # return the results
    out <- c(p0, p1, rd, logrr, logrrstar, logor)
    names(out) <- c("P0", "P1", "RD", "logRR", "logRR*", "logOR")
    out
  }

  out <- run_boot(data = data, statistic = estimator, R = R, conf = conf)

  exp_effects(out)
}


#' @rdname boot_uncond
#' @export
bootu <- boot_uncond
