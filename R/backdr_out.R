#' Compute standardized estimates with parametric outcome model
#'
#' Compute standardized estimates with parametric outcome model.
#'
#' The standardized estimates are computed using the outcome model. See chapter
#' 6, section 6.1.2, for details.
#'
#' @inheritParams backdr_out_np
#'
#' @importFrom dplyr count group_by mutate summarize filter pull relocate
#' @importFrom stats glm fitted predict
#' @importFrom rlang .data
#' @importFrom broom tidy
#'
#' @seealso effect_measures
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_out <- function(data, formula = Y ~ `T` + A + H, exposure.name = "T") {
  stopifnot(length(exposure.name) == 1)

  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  fit <- glm(formula = formula, family = "binomial", data = data)

  # counterfactual dataset with everyone untreated
  dat0 <- data
  dat0[, exposure.name] <- 0

  # counterfactual dataset with everyone untreated
  dat1 <- data
  dat1[, exposure.name] <- 1

  # compute the expected potential outcome for
  # each participant if untreated
  # NOTE: fitted() is the same as using predict(..., type = "response")
  #       BUT fitted only use the ORIGINAL data, there is no newdata.
  EYhat0 <- predict(fit, newdata = dat0, type = "response")
  # compute the expected potential outcome for
  # each participant if treated
  # NOTE: fitted() is the same as using predict(..., type = "response")
  #       BUT fitted only use the ORIGINAL data, there is no newdata.
  EYhat1 <- predict(fit, newdata = dat1, type = "response")

  # estimate the average potential outcomes
  EY0 <- mean(EYhat0)
  EY1 <- mean(EYhat1)

  # compute the effect measures
  out <- effect_measures(val0 = EY0, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = out,
    std.err = NA_real_
  )
}

#' @rdname backdr_out
#' @export
standout <- backdr_out
