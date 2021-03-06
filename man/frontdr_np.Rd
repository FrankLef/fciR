% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frontdr.R
\name{frontdr_np}
\alias{frontdr_np}
\alias{frontdoor.r}
\title{Estimate the Effect Using the Front-Door Method}
\usage{
frontdr_np(data, formula = Y ~ A + S, exposure.name = "A")

frontdoor.r(data, formula = Y ~ A + S, exposure.name = "A")
}
\arguments{
\item{data}{Dataframe of raw data.}

\item{formula}{Formula representing the model.}

\item{exposure.name}{Name of exposure variable. The other independent
variable in the formula will be assumed to be the surrogate. there can be
only one exposure and one surrogate.}
}
\value{
Dataframe in a useable format for \code{rsample::bootstraps}.
}
\description{
Estimate the effect using Front-door method.
}
\details{
Estimate the effect using Front-door method. The exposure variable is used
as a confounder in the front-door method.
}
