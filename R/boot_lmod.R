#' Estimate s sampling distribution by Bootstrapping
#'
#' Estimate s sampling distribution by bootstrapping.
#'
#  Estimate a sampling distribution by bootstrapping using \code{boot::boot()}
#'
#' @param data Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond Formula of condition.
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval
#'
#' @importFrom stats coef glm plogis coef glm
#'
#' @return Vector of summarized results
#' @export
boot_lmod <- function(data, formula = Y ~ `T` + A + H,
                     cond = Y ~ `T` + A + H,
                     R = 1000, conf = 0.95) {
  # extract the variable names form the formula
  fvars <- formula2vars(cond)

  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = "binomial", data = dat))
    # use cond to identify conditioned variables.
    # i.e. the user can decide not to use all variables from the formula
    # or, in other words, condition on some variables
    coefs <- coefs[c(fvars$x0, fvars$ind)]
    p <- sum(coefs)
    c("logitP" = unname(p))
  }

  out <- run_boot(data = data, statistic = estimator, R = R, conf = conf)

  out <- invlogit_effects(out)

  out
}

#' @rdname boot_lmod
#' @export
lmodboot <- boot_lmod
