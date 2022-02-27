#' Bootstrap and generate a dataframe of estimates with CI
#'
#' Bootstrap and generate a dataframe of estimates with CI.
#'
#' Generate a dataframe of estimates with the columns
#' \code{c("est", "conf", "lci", "uci")}.
#'
#' @param data Dataframe of raw data.
#' @param statistic Function applied to data by bootstrapping.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval width. Default is 0.95.
#' @param ... Other named arguments for \code{statistics}.
#'
#' @return Dataframe of estimates with CI.
#' @export
boot_run <- function(data, statistic, R = 1000, conf = 0.95, ...) {
  stopifnot(R >= 1, conf > 0, conf < 1)

  # run the bootstrapping
  boot.out <- boot::boot(data = data, statistic = statistic, R = R, ...)

  # extract the estimated values and confidence intervals from the boot object
  out <- sapply(X = seq_along(boot.out$t0), FUN = function(i) {
    est <- boot.out$t0[i]
    ci <- boot::boot.ci(boot.out, conf = conf, type = "norm", index = i)$normal
    out <- c(est, ci)
    names(out) <- c("est", "conf", "lci", "uci")
    out
  })

  # create the dataframe to hold the results
  out <- data.frame(t(out))
  # add the first column as the names of the results
  data.frame(name = names(boot.out$t0), out)
}

#' Extract the name of variables and coefficients from a formula
#'
#' @param formula Object of class \code{formula}.
#'
#' @importFrom stats terms
#'
#' @return List of variable names
#' @export
#'
#' @examples
#' formula2vars(Y ~ A + X1*X2)
formula2vars <- function(formula) {
  stopifnot(inherits(formula, "formula"))

  # x0 is the coefficient for intercept, used by lm, glm, geeglm, etc.
  x0 <- "(Intercept)"

  vars <- all.vars(formula)
  nvars <- length(vars)
  stopifnot(nvars >= 2)

  # outcome and treatmaent variables
  y <- vars[1]  # outcome variable
  t <- vars[2]  # treatment variable

  # if only 2 variables, t is the independent variable
  if (nvars == 2) return(list("y" = y, "t" = t, "ind" = t, "x0" = x0))

  # the condition variables
  h <- vars[3:nvars]

  # all independent variables including interactions
  ind <- attr(stats::terms(formula), which = "term.labels")
  # the condition variables including interaction with the treatment var
  ht <- ind[seq_along(ind)[-1]]

  # return results in a list
  list("y" = y, "t" = t, "h" = h, "ht" = ht, "ind" = ind, "x0" = x0)
}
