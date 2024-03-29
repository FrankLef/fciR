% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc_beta_effect_measures.R
\name{mc_beta_effect_measures}
\alias{mc_beta_effect_measures}
\title{Monte Carlo Sim of Effect Measures using the Beta distribution}
\source{
Section 4.2.1
}
\usage{
mc_beta_effect_measures(
  shape1_prms = 1,
  shape2_prms = 1,
  nrep = 5000,
  constrained = FALSE
)
}
\arguments{
\item{shape1_prms}{Vector of shape 2 parameter for the \code{rbeta} function}

\item{shape2_prms}{Vector of shape 2 parameter for the \code{rbeta} function}

\item{nrep}{Nb of repetitions}

\item{constrained}{Logical. If \code{FALSE} the data is not constrained.
Otherwise it is constrained. That is when \code{RR0} and \code{RR1} are on
different sides of 1, the data point is excluded.}
}
\value{
List of matrices. One per event of effect measure.
}
\description{
Monte Carlo Sim of Effect Measures using the Beta distribution.
}
\details{
Perform a Monte Carlo simulation of effect measures using a grid
of \code{shape1} and \code{shape2} parameters. See the \code{rbeta()} function
for more details
}
\examples{
\dontrun{
mc_beta_effect_measures(shape1 = c(0.5, 1, 3, 5, 7),
 shape2 = c(0.5, 1, 3, 5, 7), nrep = 5000)
}
}
