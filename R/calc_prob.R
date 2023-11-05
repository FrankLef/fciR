#' Calculate Probabilities
#'
#' Calculate probabilities.
#'
#' Calculate the probabilities of a variable given a dataset.
#'
#' @param data Dataframe.
#' @param var.name Name of variable to compute probabilities.
#' @param prob.name Name of column holding the probabilities' values.
#'
#' @seealso calc_prob_cond
#'
#' @return Dataframe with grouped variable \code{var.name} and probabilities in
#' \code{prob.nam}
#' @export
#'
#' @examples
#' calc_prob(airquality, var.name = "Temp", prob.name = "prob")
calc_prob <- function(data, var.name, prob.name = "prob") {
  data |>
    dplyr::group_by(across(all_of(var.name))) |>
    dplyr::summarize(!!prob.name := n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(!!prob.name := .data[[prob.name]] / sum(.data[[prob.name]]))
}


#' Calculate Probabilities Conditional on Other Variables
#'
#' Calculate probabilities conditional on other variables.
#'
#' Calculate the empirical distribution of a variable by different
#' conditions/groups.
#'
#' @param condition.names Character vector of condition variables.
#' @inheritParams calc_prob
#'
#' @seealso calc_prob
#'
#' @return Dataframe with grouped variable \code{var.name} and probabilities in
#' \code{prob.nam}
#' @export
#'
#' @examples
#' calc_prob_cond(airquality, condition.names = "Month",
#'  var.name = "Temp", prob.name = "prob")
calc_prob_cond <- function(data, condition.names, var.name,
                                prob.name = "prob") {
  data |>
    dplyr::nest_by(across(all_of(condition.names)), .key = "df") |>
    dplyr::mutate(!!prob.name := list(fciR::calc_prob(.data$df, var.name = var.name,
                                                     prob.name = prob.name))) |>
    dplyr::select(all_of(c(condition.names, prob.name))) |>
    tidyr::unnest(all_of(prob.name))
}
