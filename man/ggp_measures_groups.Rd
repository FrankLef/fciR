% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggp_measures_groups.R
\name{ggp_measures_groups}
\alias{ggp_measures_groups}
\title{Plot of effect measures by group}
\usage{
ggp_measures_groups(
  data,
  group = "model",
  title = "Title",
  subtitle = "Subtitle",
  vline = list(colors = c("lightseagreen", "violet"), linetype = "solid", size = 3, alpha
    = 0.5),
  pointrange = list(size = 1, fatten = 2),
  text = list(size = 3, color = "navy", digits = 2),
  text_size = list(title = 0.9, y_axis = 0.9)
)
}
\arguments{
\item{data}{Data frame with effect measures.}

\item{group}{Name of group variable.}

\item{title}{Title.}

\item{subtitle}{Subtitle.}

\item{vline}{geom_vline specs.}

\item{pointrange}{geom_pointrange specs.}

\item{text}{geom_text_repel specs.}

\item{text_size}{text size for title and axis as a factor of base font size.}
}
\value{
ggplot
}
\description{
Plot of effect measures by group.
}
\details{
Plot of effect measures by group. Usually with different method such as
DiD and outcome.  See exercise 7.1 and 7.2 for examples.
}
