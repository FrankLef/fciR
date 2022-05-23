#' Estimate Effect Using Instrument Variables via Logistic Fit
#'
#' Estimate effect using instrument variables via logistic fit.
#'
#' See section 9.3 p. 167-168 for details.
#'
#' @inheritParams instr_vars
#' @param niter Number of iterations
#'
#' @importFrom formulaic create.formula
#' @importFrom stats lm glm fitted predict
#' @importFrom AER ivreg
#'
#' @return Numeric vector of estimates.
#' @export
instr_logistic <- function(data, outcome.name = "Y", exposure.name = "A",
                        instrument.name = "T", tol = .Machine$double.eps^0.5,
                        niter = 10L) {
  stopifnot(tol >= .Machine$double.eps^0.5)
  stopifnot(niter >= 1, niter <= 20)

  A <- data[, exposure.name]
  Z <- data[, instrument.name]


  # Estimate D eta
  input.names <- c(exposure.name, instrument.name)
  interactions <- list(c(exposure.name, instrument.name))
  a_formula <- formulaic::create.formula(outcome.name = outcome.name,
                                         input.names = input.names,
                                         interactions = interactions,
                                         dat = data)
  mod.out <- glm(formula = a_formula, data = data, family = "binomial")
  Deta <- predict(mod.out, type = "link")

  # initialize beta_t to keep track of results
  betat <- 0
  for (i in seq_len(niter)) {
    # find Ystar and Astar from Taylor series linearization
    tmp <- exp(Deta - A * betat) / (1 + exp(Deta - A * betat))
    Ystar <- tmp * (1 + A * betat * (1 - tmp))
    Astar <- A * tmp * (1 - tmp)
    # solve the IV estimating equation for the current iteration
    betat.fit <- AER::ivreg(formula = Ystar ~ Astar, instruments = ~ Z)
    betat.new <- coef(betat.fit)[2]
    # cat("\n")
    # print(c(betat, betat.new))
    # cat("\n")

    # stop and return result if tolerance is met
    if (abs(betat - betat.new) > tol) {
      betat <- betat.new
    } else {
      betat <- betat.new
      break
    }
  }
  beta <- unname(betat)

  EY1 <- mean((exp(Deta) / (1 + exp(Deta)))[A == 1])
  EY0 <- mean((exp(Deta - A * beta) / (1 + exp(Deta - A * beta)))[A == 1])
  # cat("\n")
  # print(c(EY0, EY1))
  # cat("\n")

  effect_measures(val0 = EY0, val1 = EY1)
}