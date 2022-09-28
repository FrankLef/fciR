#' Estimate Using Structural Nested Mean Models
#'
#' Estimate using structural nested mean models.
#'
#' Estimate using structural nested mean models with he tidyverse way of coding.
#'
#' @param data Dataframe.
#' @param outcome.name Name of outcome variable.
#' @param exposure.names Name of exposure variables. There must be 2 of them.
#' @param confound.names Names of the confound variables.
#'
#' @source Section 13.2
#'
#' @return Dataframe in a useable format for \code{rsample::bootstraps}.
#' @export
#'
#' @examples
#' # An example can be found in the location identified in the
#' # source section above at the github site
#' # https://github.com/FrankLef/FundamentalsCausalInference.
time_snmm <- function(data, outcome.name, exposure.names, confound.names) {
  # exposure names must be in sequential order as in c("A!", "A2")
  stopifnot(length(exposure.names) == 2)

  # fit the saturated model for the second level of the nest
  nest2_forml <- paste(outcome.name,
                       paste(c(exposure.names, confound.names),
                             collapse = "*"),
                       sep = "~")
  # nest2_forml_old <- "Y ~ A1 + H2 + A2 + A1 * H2 + A1 * A2 + H2 * A2 + A1 * H2 * A2"
  nest2_fit <- lm(formula = nest2_forml, data = data)
  b2 <- coef(nest2_fit)

  # estimate the potential outcome of setting A2 to zero
  datY <- data[, outcome.name]
  datA1 <- data[, exposure.names[1]]
  datA2 <- data[, exposure.names[2]]
  datH2 <- data[, confound.names]
  Y1hat <- datY -
    b2["A2"] * datA2 -
    b2["A1:A2"] * datA1 * datA2 -
    b2["A2:H2"] * datH2 * datA2 -
    b2["A1:A2:H2"] * datA1 * datH2 * datA2

  # fit the model for the first level of the nest
  nest1_fit <- lm(Y1hat ~ datA1)
  b1 <- coef(nest1_fit)[2]
  out <- c("B20" = unname(b2["A2"]),
           "B20+B22" = sum(b2[c("A2", "A2:H2")]),
           "B20+B21" = sum(b2[c("A2", "A1:A2")]),
           "B20+B21+B22+B23" = sum(b2[c("A2", "A1:A2", "A2:H2", "A1:A2:H2")]),
           "B1" = unname(b1))
  # output compatible with boostrap function
  data.frame(
    term = names(out),
    estimate = unname(out),
    std.err = NA_real_)
  }

#' @rdname time_msm
#' @export
snmm.r <- time_snmm
