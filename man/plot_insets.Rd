% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_insets.R
\name{plot_insets}
\alias{plot_insets}
\title{Inset plots}
\usage{
plot_insets(
  data,
  plotfun,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  facets = NULL
)
}
\arguments{
\item{data}{A data frame}

\item{plotfun}{The function to be applied to (each facet of) the data frame to produce an inset plot. Make sure that you fix all arguments in the body of the function because this function only passed \code{data} to \code{plotfun}}

\item{xmin, xmax, ymin, ymax}{Coordinates of the inset plots on the main plots}

\item{facets}{The variable to facet by. Defaults to NULL for no facets.}
}
\value{
A list of plots
}
\description{
Make a list of inset plots suitable to add to some other (facetted) plot
}
