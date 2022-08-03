#' Estimate Effect Using Instrument Variables via Logarithmic fit
#'
#' Estimate effect using instrument variables via logarithmic fit.
#'
#' See section 9.3 p. 167-168 for details.
#'
#' @inheritParams instr_vars
#' @param niter Number of iterations
#'
#' @importFrom stats lm glm fitted predict
#' @importFrom AER ivreg
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
instr_loglinear <- function(data, formula = Y ~ A * `T`, exposure.name = "A",
                        tol = .Machine$double.eps^0.5, niter = 10L) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assertNames(exposure.name, subset.of = names(data))
  checkmate::assertInteger(niter, lower = 1, upper = 20)

  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, nvars = 1)
  outcome.name <- var_names$outcome.name
  instrument.name <- var_names$extra.names

  A <- data[, exposure.name]
  Z <- data[, instrument.name]


  # Estimate D eta
  mod.out <- glm(formula = formula, data = data, family = "poisson")
  Deta <- predict(mod.out, type = "link")

  # initialize beta_t to keep track of results
  betat <- 0
  for (i in seq_len(niter)) {
    # find Ystar and Astar from Taylor series linearization
    Ystar <- exp(Deta - A * betat) * (1 + A * betat)
    Astar <- A * exp(Deta - A * betat)
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

  EY1 <- mean(exp(Deta)[A == 1])
  EY0 <- mean(exp(Deta - A * beta)[A == 1])
  # cat("\n")
  # print(c(EY0, EY1))
  # cat("\n")

  out <- effect_measures(val0 = EY0, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname instr_loglinear
#' @export
ivlog.r <- instr_loglinear
