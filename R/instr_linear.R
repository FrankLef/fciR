#' Estimate Effect Using Instrument Variables
#'
#' Estimate effect using instrument variables.
#'
#' See section 9.3 p. 167 for details.
#'
#' @inheritParams instr_vars
#'
#' @importFrom stats lm glm fitted predict
#' @importFrom AER ivreg
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
instr_linear <- function(data, formula = Y ~ A * `T`, exposure.name = "A",
                         tol = .Machine$double.eps^0.5) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assertNames(exposure.name, subset.of = names(data))

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, nvars = 1)
  outcome.name <- var_names$outcome.name
  instrument.name <- var_names$extra.names

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
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname instr_linear
#' @export
ividentity.r <- instr_linear
