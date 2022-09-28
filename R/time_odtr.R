#' Optimal Dynamic Treatment Regime: Step 1
#'
#' Optimal dynamic treatment regime: Step 1.
#'
#' Optimal dynamic treatment regime: Step 1, compute the summary by group of
#' input variables with the proportions of outcome by group.
#'
#' @param data Dataframe.
#' @param outcome.name Name of outcome variable.
#' @param A1 Name of A1 variable.
#' @param A2 Name of A2 variable.
#' @param H2 Name of H2 variable.
#'
#' @source Section 13.3
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_odtr_prop <- function(data, outcome.name, A1, A2, H2) {
  input.names <- c(A1, A2, H2)
  data |>
    dplyr::group_by(across(all_of(input.names))) |>
    dplyr::summarize(
      freq = n(),
      freqy = sum(.data[[outcome.name]]),
      prop = .data$freqy / .data$freq)
}
#' @rdname time_odtr_prop
#' @export
mkcogtab.r <- time_odtr_prop


#' Optimal Dynamic Treatment Regime: Step 2
#'
#' Optimal dynamic treatment regime: Step 2.
#'
#' Optimal dynamic treatment regime: Step 2, compute the optimal proportions of
#' A2.
#'
#' @inheritParams time_odtr_prop
#'
#' @source Section 13.3
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_odtr_optA2 <- function(data, A1, A2, H2) {
  input.names = c(A1, H2)
  data |>
    group_by(across(all_of(input.names))) |>
    mutate(propA2opt = max(.data$prop),
           A2opt = .data[[A2]][match(.data$propA2opt, .data$prop)]) |>
    dplyr::relocate(.data$propA2opt, .after = tidyselect::last_col())
}
#' @rdname time_odtr_optA2
#' @export
A2opt.r <- time_odtr_optA2


#' Optimal Dynamic Treatment Regime: Step 3
#'
#' Optimal dynamic treatment regime: Step 3.
#'
#' Optimal dynamic treatment regime: Step 3, compute the optimal proportions of
#' A1 and A2.
#'
#' @inheritParams time_odtr_prop
#'
#' @source Section 13.3
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_odtr_optA1A2 <- function(data, A1, A2, H2) {
  optA1A2 <- data |>
    group_by(.data[[A1]]) |>
    mutate(nA1 = sum(.data$freq)) |>
    group_by(across(all_of(c(A1, H2)))) |>
    mutate(nA1H2 = sum(.data$freq),
           propA1H2 = .data$nA1H2 / .data$nA1,
           margA1H2 = .data$propA2opt * .data$propA1H2) |>
    group_by(across(all_of(c(A1, A2)))) |>
    summarize(probA1A2 = sum(.data$margA1H2)) |>
    distinct(.data[[A1]], .data$probA1A2) |>
    ungroup() |>
    filter(.data$probA1A2 == max(.data$probA1A2)) |>
    rename(A1opt = A1,
           propA1opt = .data$probA1A2) |>
    identity()
  # optA1A2

  optA1A2_repeat <-
    bind_rows(replicate(nrow(data), optA1A2, simplify = FALSE))

  data |>
    bind_cols(optA1A2_repeat)
}
#' @rdname time_odtr_optA1A2
#' @export
A1opt.r <- time_odtr_optA1A2




#' Optimal Dynamic Treatment Regime: Step 4
#'
#' Optimal dynamic treatment regime: Step 4.
#'
#' Optimal dynamic treatment regime: Step 4, extract the optimal regime.
#'
#' @inheritParams time_odtr_prop
#'
#' @source Section 13.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_odtr_optimal <- function(data, A1, A2, H2) {
  the_A1opt <- data$A1opt[1]

  df <- data |>
    filter(.data[[A1]] == the_A1opt) |>
    distinct(.data$A1opt, .data$propA1opt,
             .data[[A1]], .data[[H2]],
             .data$A2opt, .data$propA2opt)

  out <- c(
    "A1opt" = df$A1opt[1],
    "propA1opt" = df$propA1opt[1],
    "A2optH20" = df$A2opt[df[, H2] == 0],
    "propA2optH20" = df$propA2opt[df[, H2] == 0],
    "A2optH21" = df$A2opt[df[, H2] == 1],
    "propA2optH21" = df$propA2opt[df[, H2] == 1])

  # output compatible with boostrap function
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_)
}

#' @rdname time_odtr_optimal
#' @export
optimal.r <- time_odtr_optimal


#' Optimal Dynamic Treatment Regime: All steps
#'
#' Optimal dynamic treatment regime: All steps.
#'
#' Optimal dynamic treatment regime: All steps, extract the optimal regime.
#'
#' @inheritParams time_odtr_prop
#'
#' @source Section 13.3
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_odtr <- function(data, outcome.name, A1, A2, H2) {
  data |>
    fciR::time_odtr_prop(outcome.name = outcome.name, A1 = A1, A2 = A2, H2 = H2) |>
    fciR::time_odtr_optA2(A2 = A2, A1 = A1, H2 = H2) |>
    fciR::time_odtr_optA1A2(A1 = A1, A2 = A2, H2 = H2) |>
    fciR::time_odtr_optimal(A1 = A1, H2 = H2)
}
