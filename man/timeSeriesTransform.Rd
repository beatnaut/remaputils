% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analytics.R
\name{timeSeriesTransform}
\alias{timeSeriesTransform}
\title{Provides for a time series smoothing, date ascension and limit expansion}
\usage{
timeSeriesTransform(df, smooth = 0.3, limitFactor = 0.1)
}
\arguments{
\item{df}{A data frame with 'value' and 'date' columns.}

\item{smooth}{The smoothing parameter.}

\item{limitFactor}{The factor with which to expand the limits.}
}
\value{
A list with the date-ascended and smoothed values and the expanded lower and upper limits.
}
\description{
This is the description
}
