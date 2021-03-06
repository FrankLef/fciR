% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/did_loglinear.R
\name{did_loglinear}
\alias{did_loglinear}
\alias{didloglinear}
\title{Compute the DiD Estimator with a Loglinear Model}
\usage{
did_loglinear(
  data,
  formula = Y1 ~ T + Y0,
  exposure.name = "T",
  names_to = "var",
  timevar = "time"
)

didloglinear(
  data,
  formula = Y1 ~ T + Y0,
  exposure.name = "T",
  names_to = "var",
  timevar = "time"
)
}
\arguments{
\item{data}{Dataframe of raw data.}

\item{formula}{Formula representing the model.}

\item{exposure.name}{Name of exposure variable. All other independent
variables in the formula will be assumed to be confounds.}

\item{names_to}{Name that will be used for the name variable.}

\item{timevar}{Name that will be used for the time variable.}
}
\value{
Dataframe in a useable format for \code{rsample::bootstraps}.
}
\description{
Compute the DiD Estimator with a loglinear model.
}
\details{
the DiD Estimator using the scripts from section 7.2, p. 141-142.
}
\seealso{
did_longer
}
