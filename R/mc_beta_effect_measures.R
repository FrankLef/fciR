#' Monte Carlo Sim of Effect Measures using the Beta distribution
#'
#' Monte Carlo Sim of Effect Measures using the Beta distribution.
#'
#' Perform a Monte Carlo simulation of effect measures using a grid
#' of \code{shape1} and \code{shape2} parameters. See the `rbeta()` function
#' for more details
#'
#' @param shape1_prms Vector of shape 2 parameter for the `rbeta` function
#' @param shape2_prms Vector of shape 2 parameter for the `rbeta` function
#' @param nrep Nb of repetitions
#' @param constrained Logical. If \code{FALSE} the data is not constrained.
#' Otherwise it is constrained. That is when \code{RR0} and \code{RR1} are on
#' different sides of 1, the data point is excluded.
#'
#' @importFrom stats rbeta
#'
#' @return List of matrices. One per event of effect measure.
#' @export
#'
#' @examples
#' \dontrun{
#' mc_beta_effect_measures(shape1 = c(0.5, 1, 3, 5, 7),
#'  shape2 = c(0.5, 1, 3, 5, 7), nrep = 5000)
#' }
mc_beta_effect_measures <- function(shape1_prms = 1, shape2_prms = 1, nrep = 5000,
                                    constrained = FALSE) {
  stopifnot(nrep >= 1)
  # the shape parameters must be >= 0.5 to maintain
  # the positivity assumption
  stopifnot(all(shape1_prms >= 0.5), all(shape2_prms >= 0.5))

  # the function used by Monte Carlo for each parameter in the grid
  calc_measures <- function(shape1, shape2) {

    # get the random sample from beta distribution
    s <- rbeta(n = 4, shape1 = shape1, shape2 = shape2)
    EY_T0_M0 <- s[1]
    EY_T1_M0 <- s[2]
    EY_T0_M1 <- s[3]
    EY_T1_M1 <- s[4]

    # IMPORTANT: Make sure the positivity assumption is met.
    stopifnot(EY_T0_M0 > 0, EY_T0_M0 < 1, EY_T1_M0 > 0, EY_T1_M0 < 1,
              EY_T0_M1 > 0, EY_T0_M1 < 1, EY_T1_M1 > 0, EY_T1_M1 < 1)

    # compute the effect measures
    RD0 <- EY_T1_M0 - EY_T0_M0
    RD1 <- EY_T1_M1 - EY_T0_M1
    RR0 <- EY_T1_M0 / EY_T0_M0
    RR1 <- EY_T1_M1 / EY_T0_M1
    RR0star <- (1 - EY_T0_M0) / (1 - EY_T1_M0)
    RR1star <- (1 - EY_T0_M1) / (1 - EY_T1_M1)
    OR0 <- RR0 * RR0star
    OR1 <- RR1 * RR1star

    # How did the measures change with the modifier?
    RD <- sign(RD1 - RD0)
    RR <- sign(RR1 - RR0)
    RRstar <- sign(RR1star - RR0star)
    OR <- sign(OR1 - OR0)

    # We want to know if the various subsets of the four causal measures would
    # agree (i.e. change together from one stratum to the other)

    # 1 pair only
    RD_RR <-
      (RD == RR) & (RD != RRstar) & (RD != OR) &
      (RR != RRstar) & (RR != OR) & (RRstar != OR)
    RD_RRstar <-
      (RD != RR) & (RD == RRstar) & (RD != OR) &
      (RR != RRstar) & (RR != OR) & (RRstar != OR)
    RD_OR <-
      (RD != RR) & (RD != RRstar) & (RD == OR) &
      (RR != RRstar) & (RR != OR) & (RRstar != OR)
    RR_RRstar <-
      (RD != RR) & (RD != RRstar) & (RD != OR) &
      (RR == RRstar) & (RR != OR) & (RRstar != OR)
    RR_OR <-
      (RD != RR) & (RD != RRstar) & (RD != OR) &
      (RR != RRstar) & (RR == OR) & (RRstar != OR)
    RRstar_OR <-
      (RD != RR) & (RD != RRstar) & (RD != OR) &
      (RR != RRstar) & (RR != OR) & (RRstar == OR)
    # 2 pairs not being 3-wise
    # called "Opposite pairwise events"
    RD_RR_vs_RRstar_OR <-
      (RD == RR) & (RD != RRstar) & (RD != OR) &
      (RR != RRstar) & (RR != OR) & (RRstar == OR)
    RD_RRstar_vs_RR_OR <-
      (RD != RR) & (RD == RRstar) & (RD != OR) &
      (RR != RRstar) & (RR == OR) & (RRstar != OR)
    RD_OR_vs_RR_RRstar <-
      (RD != RR) & (RD != RRstar) & (RD == OR) &
      (RR == RRstar) & (RR != OR) & (RRstar != OR)
    # 3-wise
    RD_RR_RRstar <-
      (RD == RR) & (RD == RRstar) & (RD != OR) &
      (RR == RRstar) & (RR != OR) & (RRstar != OR)
    RD_RR_OR <-
      (RD == RR) & (RD != RRstar) & (RD == OR) &
      (RR != RRstar) & (RR == OR) & (RRstar != OR)
    RD_RRstar_OR <-
      (RD != RR) & (RD == RRstar) & (RD == OR) &
      (RR != RRstar) & (RR != OR) & (RRstar == OR)
    RR_RRstar_OR <-
      (RD != RR) & (RD != RRstar) & (RD != OR) &
      (RR == RRstar) & (RR == OR) & (RRstar == OR)
    # 4-wise
    RD_RR_RRstar_OR <-
      (RD == RR) & (RD == RRstar) & (RD == OR) &
      (RR == RRstar) & (RR == OR) & (RRstar == OR)
    # All measures move in different directions
    # This is an impossible event and represent the empty set
    NONE <-
      (RD != RR) & (RD != RRstar) & (RD != OR) &
      (RR != RRstar) & (RR != OR) & (RRstar != OR)

    # create the output list
    out <- list("RD_RR" = RD_RR, "RD_RRstar" = RD_RRstar, "RD_OR" = RD_OR,
                "RR_RRstar" = RR_RRstar, "RR_OR" = RR_OR, "RRstar_OR" = RRstar_OR,
                "RD_RR_vs_RRstar_OR" = RD_RR_vs_RRstar_OR,
                "RD_RRstar_vs_RR_OR" = RD_RRstar_vs_RR_OR,
                "RD_OR_vs_RR_RRstar" = RD_OR_vs_RR_RRstar,
                "RD_RR_RRstar" = RD_RR_RRstar, "RD_RR_OR" = RD_RR_OR,
                "RD_RRstar_OR" = RD_RRstar_OR, "RR_RRstar_OR" = RR_RRstar_OR,
                "RD_RR_RRstar_OR" = RD_RR_RRstar_OR, "NONE" = NONE)


    # if constrained and condition is met, set to NA
    # is constrained the set to NA
    # if RR0 and RR1 are on different side of 1
    # then the constraint is met
    cond <- sign(RR0 - 1) != sign(RR1 - 1)
    if (constrained & cond) out[seq_along(out)] <- NA

    out
  }

  # run parametric Monte Carlo simulation
  params <- list("shape1" = shape1_prms, "shape2" = shape2_prms)
  mc.out <- MonteCarlo::MonteCarlo(func = calc_measures, nrep = nrep,
                                   param_list = params)

  # output the results. One matrix per event where every matrix element
  # is a percentage frequency for given shape1 and shape2 parameters
  out <- lapply(mc.out$results, FUN = function(x) {
    m <- sapply(
      X = seq_len(dim(x)[1]), FUN = function(s1) {
        sapply(X = seq_len(dim(x)[2]), function(s2) {
          # since the constrained items are set to NA, we must
          # remove NA from the calculations, only has an effect
          # when constrained = TRUE
          nb <- sum(!is.na(x[s1, s2, ]))
          sum(x[s1, s2, ], na.rm = TRUE) / nb
        })
      })
    # Convert array to matrix and name row and columns
    m <- as.matrix(m)
    rownames(m) <- paste("s2", shape2_prms, sep = "=")
    colnames(m) <- paste("s1", shape1_prms, sep = "=")
    m
  })

  out
}
