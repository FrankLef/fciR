#' Compute estimates of the conditional association measures
#'
#' Compute estimates of the conditional association measures.
#'
#' Estimate the expected conditional outcomes and the
#' conditional effect or association measures.
#' IMPORTANT: This is the function in chapter 3 called lmodboot.r
#'            It has been renamed bootc.r to avoid conflict with
#'            lmodboot.r of chapter 2
#'
#' @param data Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond0 Formula of condition 0.
#' @param cond1 Formula of condition 1.
#' @param family Family used by \code{glm}. Default is "binomial".
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Dataframe of effect measures.
boot_cond <- function(data, formula = Y ~ `T` + A + H,
                  cond0 = Y ~ A + H,
                  cond1 = Y ~ `T` + A + H,
                  family = c("binomial", "poisson", "gaussian"),
                  R = 1000, conf = 0.95) {
  # the family used by glm
  family <- match.arg(family)

  # extract the variables names from the formula
  fvars0 <- formula2vars(cond0)
  fvars1 <- formula2vars(cond1)


  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = family, data = dat))
    # use variables from cond0 and cond1 to identify conditioned variables.
    xbeta0 <- sum(coefs[c(fvars0$x0, fvars0$ind)])
    xbeta1 <- sum(coefs[c(fvars1$x0, fvars1$ind)])

    P0 <- plogis(xbeta0)  # plogis is the inverse of logit
    P1 <- plogis(xbeta1)  # plogis is the inverse of logit

    # calculate effect measures
    calc_effect_measures(val0 = P0, val1 = P1)
  }

  out <- run_boot(data = data, statistic = estimator, R = R, conf = conf)

  exp_effects(out)  # exponentiate effect measures
}

#' @rdname boot_cond
#' @export
bootc <- boot_cond
