% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_quant.R
\name{prop_quant}
\alias{prop_quant}
\title{Stratifying on the Quantiles of the Propensity Score}
\source{
Section 10.3. p. 180.
}
\usage{
prop_quant(
  data,
  formula,
  exposure.name,
  confound.names,
  probs = 0:4/4,
  quant_var = "pquants"
)
}
\arguments{
\item{data}{Dataframe of raw data.}

\item{formula}{Formula representing the model.}

\item{exposure.name}{Name of exposure variable.}

\item{confound.names}{Names of the confound variables.}

\item{probs}{Vector of probability for the quantiles.}

\item{quant_var}{Name of the quantile variable. Default is "pquants".}
}
\value{
Dataframe in a useable format for \code{rsample::bootstraps}.
}
\description{
Stratifying on the quantiles of the propensity score.
}
\details{
Stratifying on the quantiles is useful to identify the possible interactions
between the propensity scores and the exposure.
}
\examples{
# An example can be found in the location identified in the
# source section above at the github site
# https://github.com/FrankLef/FundamentalsCausalInference.
}
