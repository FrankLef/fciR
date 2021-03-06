% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backdr_out.R
\name{backdr_out}
\alias{backdr_out}
\alias{standout}
\title{Compute standardized estimates with parametric outcome model}
\usage{
backdr_out(
  data,
  formula = Y ~ T + A + H,
  exposure.name = "T",
  family = c("binomial", "poisson", "gaussian")
)

standout(
  data,
  formula = Y ~ T + A + H,
  exposure.name = "T",
  family = c("binomial", "poisson", "gaussian")
)
}
\arguments{
\item{data}{Dataframe of raw data.}

\item{formula}{Formula representing the model.}

\item{exposure.name}{Name of exposure variable. All other independent
variables in the formula will be assumed to be confounds.}

\item{family}{Name of the model's family. Must be one of
\code{c("binomial", "poisson", "gaussian")}. default is \code{"binomial"}.}
}
\value{
Dataframe in a useable format for \code{rsample::bootstraps}.
}
\description{
Compute standardized estimates with parametric outcome model.
}
\details{
The standardized estimates are computed using the outcome model. See chapter
6, section 6.1.2, for details.
}
\seealso{
effect_measures
}
