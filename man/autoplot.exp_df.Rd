% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{autoplot.exp_df}
\alias{autoplot.exp_df}
\title{Plot experience study results}
\usage{
\method{autoplot}{exp_df}(
  object,
  ...,
  mapping,
  scales = "fixed",
  geoms = c("lines", "bars"),
  y_labels = scales::label_percent(accuracy = 0.1)
)
}
\arguments{
\item{object}{An object of class \code{exp_df} usually created by the
function \code{exp_stats()}.}

\item{...}{Faceting variables passed to \code{facet_wrap()}.}

\item{mapping}{Aesthetic mapping passed to \code{ggplot()}.}

\item{scales}{The \code{scales} argument passed to \code{facet_wrap()}.}

\item{geoms}{Type of geometry. If "points" is passed, the plot will
display lines and points. If "bars", the plot will display bars.}

\item{y_labels}{Label function passed to \code{scale_y_continuous()}.}
}
\value{
a \code{ggplot} object
}
\description{
Plot experience study results
}
\details{
If no aesthetic map is supplied, the plot will use the first
grouping variable in \code{object} on the x axis and \code{q_obs} on the y
axis. In addition, the second grouping variable in \code{object} will be
used for color and fill.

If no faceting variables are supplied, the plot will use all grouping
variables 3+ as facets passed into \code{facet_wrap()}.
}
