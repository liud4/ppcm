% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppcm.R
\name{ppcm}
\alias{ppcm}
\title{Predictive Partly Conditional Model}
\usage{
ppcm(
  data,
  id.var,
  outcome.var,
  time.var,
  cov.ti,
  cov.td,
  method = NULL,
  baseline.only = FALSE,
  fixed.window.length = NULL,
  int.width = NULL,
  bandwidth = NULL,
  width.subset = FALSE
)
}
\arguments{
\item{data}{is a data frame in a long format}

\item{id.var}{is the variable name indicating ID or similar}

\item{outcome.var}{is the variable name of the outcome}

\item{time.var}{is the variable name of the follow-up time}

\item{cov.ti}{is the list of variable names of the time-independent covariates}

\item{cov.td}{is the list of variable names of the time-dependent covariates}

\item{method}{allows the users to select between the restricted \code{ppcm}
using fixed time window only and the unrestricted \code{ppcm-u} using all
available data.}

\item{baseline.only}{is a logistic argument determining whether or not to fit
the model using the baseline covariates only. The default is \code{FALSE}.}

\item{fixed.window.length}{is an integer specifying the fixed window lenght if
the \code{ppcm} method is used}

\item{int.width}{is the interval width}

\item{bandwidth}{is the bandwidth}

\item{width.subset}{is a logistic argument that determines whether or not to
subset the analysis data: \code{TRUE}, subsetting the data based on \code{int.width}
and \code{bandwidth}; \code{FALSE}, otherwise.}
}
\value{
a list of parameter estimates and standard errors.
}
\description{
Function short description
}
\references{
Neal, J. E., Zhang, P. & Liu, D. (2024). Predictive partly conditional
models for longitudinal ordinal outcomes with application to Alzheimer's disease
progression. Under review.
}
\keyword{internal}
