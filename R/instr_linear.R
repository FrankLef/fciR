#' Estimate Effect Using Instrument Variables
#'
#' Estimate effect using instrument variables.
#'
#' See section 9.3 p. 167 for details. \bold{It is important to note that the
#' formula must specify a saturated model, e.g. Y ~ T * A}.
#'
#' @inheritParams instr_vars
#' @param formula Formula representing the model. It must be a saturated model,
#' e.g. \code{Y ~ T * A}.
#'
#' @importFrom AER ivreg
#'
#' @source Section 9.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
instr_linear <- function(data, formula, exposure.name, instrument.name,
                         tol = .Machine$double.eps^0.5) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, instrument.name)
  outcome.name <- var_names$outcome.name

  mod.out <- glm(formula = formula, data = data, family = "gaussian")

  # estimate D eta
  Deta <- predict(mod.out, type = "link")
  Ystar <- Deta
  Astar <- data[, exposure.name]
  Z <- data[, instrument.name]

  # solve the IV estimating equation
  reg <- AER::ivreg(formula = Ystar ~ Astar, instruments = ~ Z)
  beta <- coef(reg)[2]

  # estimate E(Y(1)|A=1) and E(Y(0)|A=1)
  dat1 <- data[, exposure.name] == 1
  EY1 <- mean(Deta[dat1])
  EY0 <- mean((Deta - data[, exposure.name] * beta)[dat1])

  # estimate the effects
  out <- effect_measures(val0 = EY0, val1 = EY1)
  # change the names to reflect their true definition
  the_terms <- c("EY0" = r"(E(Y(0)|A=1))",
                 "EY1" = r"(E(Y|A=1))",
                 "RD" = r"(E(Y-Y(0)|A=1))",
                 "logRR" = r"(log(E(Y|A=1))-log(E(Y(0)|A=1)))",
                 "logOR" = r"(logit(E(Y|A=1))-logit(E(Y(0)|A=1)))")
  pos <- match(names(the_terms), names(out), nomatch = 0L)
  assertthat::assert_that(any(pos != 0))
  out <- out[pos]
  names(out) <- the_terms[pos != 0]
  # add beta to the results
  out <- c("beta" = unname(beta), out)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname instr_linear
#' @export
ividentity.r <- instr_linear
