#' Compute Standardized Averages Using Exposure Modeling, Non Parametric
#'
#' Compute standardized averages using exposure modeling, non-parametric.
#'
#' Compute standardized averages using exposure modeling. See introduciton of
#' section 6.2 and section 6.2.1.
#'
#' @inheritParams backdr_out_np
#'
#' @importFrom dplyr count group_by ungroup mutate summarise filter pull near
#' @importFrom rlang .data
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
backdr_exp_np <- function(data, formula = Y ~ `T` + H, exposure.name = "T",
                          att = FALSE) {
  # audit and extract the variables
  var_names <- audit_formula(data, formula, exposure.name)
  outcome.name <- var_names$outcome.name
  confound.names <- var_names$extra.names

  # get the summarized data
  summ <- data %>%
    count(.data[[outcome.name]], .data[[exposure.name]], .data[[confound.names]],
          name = "n") %>%
    mutate(freq = n / sum(n))
  stopifnot(dplyr::near(sum(summ$freq), 1))

  # compute e(H=0) and e(H=1)
  eH <- summ %>%
    group_by(.data[[exposure.name]], .data[[confound.names]]) |>
    summarize(n = sum(n)) |>
    group_by(.data[[confound.names]]) |>
    mutate(prob = n / sum(n)) %>%
    filter(.data[[exposure.name]] == 1) |>
    arrange(.data[[confound.names]]) |>
    pull(.data$prob)
  eH
  stopifnot(all(eH > .Machine$double.eps^0.5))
  eH0 <- eH[1]
  eH1 <- eH[2]

  # compute the E(T) when ATT is required
  e0 <- NA_real_
  if (att) {
    e0 <- summ %>%
      filter(.data[[exposure.name]] == 1) |>
      summarize(sum(.data$freq)) |>
      pull()
  }

  # create the  eH variable
  EY <- summ %>%
    mutate(eH = (1 - .data[[confound.names]]) * eH0 + .data[[confound.names]] * eH1)

  # compute the summand of the estimating equations with and wihtout ATT
  if (!att) {
    EY <- EY %>%
      mutate(s = (1 - .data[[exposure.name]]) * .data[[outcome.name]] / (1 - eH) +
               .data[[exposure.name]] * .data[[outcome.name]] / eH)
  } else {
    EY <- EY %>%
      mutate(s = (1 - .data[[exposure.name]]) * .data[[outcome.name]] * eH / (e0 * (1 - eH)) +
               .data[[exposure.name]] * .data[[outcome.name]] / eH )
    # E(Y(1)|T=1) = E(Y|T=1) is estimated as before
    # see very last paragraph of section 6.2.1
    EYT1 <- summ |>
      filter(.data[[exposure.name]] == 1) |>
      group_by(.data[[outcome.name]]) |>
      summarize(n = sum(.data$n)) |>
      mutate(prob = .data$n / sum(.data$n)) |>
      summarize(EYT1 = sum(.data[[outcome.name]] * .data$prob)) |>
      pull(EYT1)
  }

  # Estimate the value of the potential outcome
  EY <- EY |>
    group_by(.data[[exposure.name]]) |>
    summarize(EY = sum(.data$s * .data$freq)) |>
    arrange(.data[[exposure.name]]) |>
    pull(EY)

  EY0 <- EY[1]
  EY1 <- EY[2]
  if (att) EY1 <- EYT1  # if ATT, compute E(Y|T=1) as before

  # estimate the effect measures
  out <- effect_measures(val0 = EY0, val1 = EY1)
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_
  )
}

#' @rdname backdr_exp_np
#' @export
mk.mortdat <- backdr_exp_np

#' @rdname backdr_exp_np
#' @export
attsem.r <- backdr_exp_np
