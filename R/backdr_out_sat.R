#' Standardized estimates via Outcome Modeling, Saturated regression
#'
#' Standardized estimates via outcome modeling, saturated regression.
#'
#' The standardized estimates are computed using a saturated regression fit.
#' That is all variables and their interactions are used. \emph{make sure all
#' variables and their interactions are included in the formula}.
#' See p. 101.
#'
#'
#' @inheritParams backdr_out_np
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_out_sat <- function(data, formula, exposure.name,
                           confound.names, att = FALSE) {
  checkmate::assertDataFrame(data)
  checkmate::assertFormula(formula)
  checkmate::assert_string(confound.names, min.chars = 1)
  checkmate::assertFlag(att)

  # this function works when there is only one confound
  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name, confound.names)
  confound.names <- var_names$extra.names

  x0 <- "(Intercept)"  # name of intercept used by lm, glm, etc.

  # marginal expected value of H
  if (!att) {
    mean_confound <- mean(data[, confound.names])
  } else {
    # condition on treatment when ATT is requested
    condT1 <- data[, exposure.name] == 1
    mean_confound <- mean(data[condT1, confound.names])
  }

  # fit the outcome model
  fit <- glm(formula = formula, data = data) |>
    broom::tidy()

  # add distribution marginal expected potential outcomes
  # marginal computaiton only for terms including the confound
  fit <- fit |>
    mutate(
      # find the terms that includes the confound
      marg_exp = grepl(pattern = confound.names, x = .data$term),
      # multiply the terms inlcuding the confound
      marg_exp = ifelse(.data$marg_exp,
                        .data$estimate * mean_confound, .data$estimate)
    )

  # E(Y(0))
  EY0 <- fit |>
    filter(.data$term %in% c(x0, confound.names)) |>
    summarize(EY = sum(.data$marg_exp)) |>
    pull()
  # E(Y(1))
  EY1 <- fit |>
    summarize(EY = sum(.data$marg_exp)) |>
    pull()

  out <- effect_measures(EY0, EY1, log = FALSE)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname backdr_out_sat
#' @export
stand.r <- backdr_out_sat
