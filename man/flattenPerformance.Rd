% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{flattenPerformance}
\alias{flattenPerformance}
\title{A function to flatten the results from the performance endpoint.}
\usage{
flattenPerformance(
  x,
  containerType,
  currentDate,
  startDate,
  periodicity = "M",
  window = "Y"
)
}
\arguments{
\item{x}{The list form the performance endpoint.}

\item{containerType}{The type of container, i.e "portfolios". "shareclasses" etc.}

\item{currentDate}{The date of report.}

\item{startDate}{The start date.}

\item{periodicity}{The desired alternative periodicity of returns. 'M' for monthly, 'Q' for quarterly.}

\item{window}{The time window of analysis.}
}
\value{
A list with xtsPrice, stats, periodStats and returns.
}
\description{
This is the description
}
