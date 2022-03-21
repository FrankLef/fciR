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
#' @inheritParams backdr_out
#'
#' @importFrom formulaic create.formula
#' @importFrom stats formula lm glm fitted predict
#' @importFrom geepack geeglm
#'
#' @seealso backdr_exp
#'
#' @return Dataframe of estimates
#' @export
backdr_exp_gee <- function(data, outcome.name = "Y", exposure.name = "T",
                           confound.names = "H") {
  x0 <- "(Intercept)"  # name of intercept used by geeglm

  # exposure model formula
  eformula <- formulaic::create.formula(outcome.name = exposure.name,
                                        input.names = confound.names,
                                        dat = data)

  # IMPORTANT: geepack::geeglm returns an error when using a formula
  #            created with formulaic::create.formula! Do it with paste().
  # weighted linear model formula
  lformula <- formula(paste(outcome.name, exposure.name, sep = "~"))

  data$id <- seq_len(nrow(data))  # id column used by geeglm

  # estimate the parametric exposure model
  eH <- fitted(glm(formula = eformula, family = "binomial", data = data))
  stopifnot(all(!dplyr::near(eH, 0)))  # eH must not equal zero

  # compute the weights
  datT <- data[, exposure.name]
  data$W <- (1 / eH) * datT + (1 / (1 - eH)) * (1 - datT)

  # fit the weighted linear model
  coefs <- coef(geepack::geeglm(formula = lformula, data = data,
                                id = id, weights = W))

  # estimate the expected potential outcome
  EY0 <- coefs[x0]
  EY1 <- sum(coefs)

  # estimate the effect measures
  effect_measures(EY0, EY1)
}

#' @rdname backdr_exp_gee
#' @export
standexpgee <- backdr_exp_gee

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
backdr_exp_geeX <- function(data, formula = Y ~ `T` + H, R = 1000, conf = 0.95) {

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
