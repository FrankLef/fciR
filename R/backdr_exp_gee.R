#' Compute standardized estimates with parametric exposure model
#'
#' Compute standardized estimates with parametric exposure model.
#'
#' The standardized estimates are computed using the exposure model and the
#' \code{geeglm} from the \code{gee} package.
#' This method requires 2 different formulas which are created from the
#' arguments \code{formula}. The 2 formulas created are for the exposure model
#' and another one for the weighted linear model.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula must be in the form \code{Y ~ `T` + H}
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @importFrom stats lm glm fitted predict
#'
#' @seealso backdr_exp
#'
#' @return Dataframe of estimates
#' @export
backdr_exp_gee <- function(data, formula = Y ~ `T` + H, R = 1000, conf = 0.95) {

  # extract the variables names from the formula
  fvars <- formula2vars(formula)

  # exposure model formula
  eformula <- formula(paste(fvars$t, paste(fvars$h, collapse = "+"), sep = "~"))
  # weighted linear model formula
  lformula <- formula(paste(fvars$y, fvars$t, sep = "~"))

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    dat[, "id"] <- seq_len(nrow(dat))  # id column used by geeglm

    # estimate the parametric exposure model
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero

    # compute the weights
    datT <- dat[, fvars$t]
    dat[, "W"] <- (1 / e) * datT + (1 / (1 - e)) * (1 - datT)

    # fit the weighted linear model
    coefs <- coef(geepack::geeglm(formula = lformula, data = dat,
                                  id = id, weights = W))

    # estimate the expected potential outcome
    EY0 <- coefs[fvars$x0]
    EY1 <- sum(coefs)

    # estimate the effect measures
    effect_measures(EY0, EY1)
  }

  # run the bootstrapping
  out <- boot_run(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  effect_exp(data = out)
}

#' @rdname backdr_exp_gee
#' @export
standexpgee <- backdr_exp_gee
