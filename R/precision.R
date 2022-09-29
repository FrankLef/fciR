#' Compute Precision efficiency
#'
#' Compute precision efficiency.
#'
#' Compare the variance obtained when using a precision variable or not.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula representing the model.
#' @param exposure.name Name of exposure variable.
#' @param precision.name Name of precision variable.
#' @param none.name Name of the term when no precision variable is used.
#'
#' @source Section 11.2
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
precision_eff <- function(data, formula, exposure.name, precision.name,
                          none.name = "None") {
  # Estimate the effect without the precision variable
  vars <- all.vars(formula)
  vars <- vars[vars != precision.name]  # remove the precision variable
  formula1 <- paste(vars[[1]], vars[[2]], sep = "~")
  fit_none <- lm(formula = formula1, data = data)
  # Estimate the effect with the precision variable
  fit <- lm(formula = formula, data = data)
  est <- c(coef(fit_none)[2], coef(fit)[2])
  names(est) <- c(none.name, precision.name)
  std_err <- c(sqrt(diag(stats::vcov(fit_none)))[2],
               sqrt(diag(stats::vcov(fit)))[2])
  # must return a data.frame of tidy results to use with bootstrap later
  data.frame(
    term = names(est),
    estimate = unname(est),
    std.err = std_err
  )
}

#' @rdname precision_eff
#' @export
precision.r <- precision_eff

#' Compute Stats on Precision Efficiency
#'
#' Compute stats on precision efficiency.
#'
#' Compute stats on precision efficiency using bootstrapping.
#'
#' @param data Dataframe of raw data.
#' @param models List of models with specs.
#' @param times Nb of boostrap iterations.
#' @param seed Boostrap seed.
#' @param none.name Name of the term when no precision variable is used.
#'
#' @seealso precision_eff
#'
#' @return List with original dataframe of stats and original \code{models}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
precision_stats <- function(data, models, times = 1000, seed = NULL, none.name = "None") {
  # repeat for each model
  for (i in seq_along(models)) {
    frml <- models[[i]]$formula
    vars <- models[[i]]$vars

    # Get the fit for each model
    fit <- fciR::precision_eff(
      data, formula = frml,
      exposure.name = vars["exposure"], precision.name = vars["precision"],
      none.name = none.name)
    models[[i]]$fit <- fit

    # bootstrap the models
    boot <- fciR::boot_est(
      data = data,
      func = fciR::precision_eff,
      times = times, alpha = 0.05, seed = seed,
      formula = frml,
      exposure.name = vars["exposure"], precision.name = vars["precision"])
    models[[i]]$boot <- boot

    # create the output for each model
    models[[i]]$out <- fit |>
      dplyr::bind_cols(boot[, c(".lower", ".upper")])
  }
  # create the consolidated output
  out.conso <- lapply(seq_along(models), FUN = function(i) {
    models[[i]]$out
  })
  out.conso <- out.conso |>
    bind_rows() |>
    # remove duplicate terms ("None")
    dplyr::slice(match(unique(.data$term), .data$term))

  list("stats" = out.conso, "models" = models)
}

#' @rdname precision_stats
#' @export
bootprecision.r <- precision_stats

