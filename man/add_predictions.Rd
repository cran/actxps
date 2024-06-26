% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_predictions.R
\name{add_predictions}
\alias{add_predictions}
\title{Add predictions to a data frame}
\usage{
add_predictions(.data, model, ..., col_expected = NULL)
}
\arguments{
\item{.data}{A data frame, preferably with the class \code{exposed_df}}

\item{model}{A model object that has an S3 method for \code{\link[=predict]{predict()}}}

\item{...}{Additional arguments passed to \code{\link[=predict]{predict()}}}

\item{col_expected}{\code{NULL} or a character vector containing column names for
each value returned by \code{\link[=predict]{predict()}}}
}
\value{
A data frame or \code{exposed_df} object with one of more new columns
containing predictions.
}
\description{
Attach predicted values from a model to a data frame with
exposure-level records.
}
\details{
This function attaches predictions from a model to a data frame
that preferably has the class \code{exposed_df}. The \code{model} argument must be
a model object that has an S3 method for the \code{\link[=predict]{predict()}} function. This
method must have new data for predictions as the second argument.

The \code{col_expected} argument is optional.
\itemize{
\item If \code{NULL}, names from the result of \code{\link[=predict]{predict()}} will be used. If there are
no names, a default name of "expected" is assumed. In the event that
\code{\link[=predict]{predict()}} returns multiple values, the default name will be suffixed by
"_x", where x = 1 to the number of values returned.
\item If a value is passed, it must be a character vector of same length as
the result of \code{\link[=predict]{predict()}}
}
}
\examples{
expo <- expose_py(census_dat, "2019-12-31") |>
  mutate(surrender = status == "Surrender")
mod <- glm(surrender ~ inc_guar + pol_yr, expo, family = 'binomial')
add_predictions(expo, mod, type = 'response')

}
