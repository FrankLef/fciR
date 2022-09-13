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
  out.conso <- do.call(rbind, out.conso) |>
    as.data.frame() |>
    # remove duplicate terms ("None")
    dplyr::slice(match(unique(.data$term), .data$term))

  list("stats" = out.conso, "models" = models)
}
