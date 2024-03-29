% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{getPerformanceV2}
\alias{getPerformanceV2}
\title{A function to generate a rich performance data for a container including
benchmark and relative performance.}
\usage{
getPerformanceV2(
  containerType,
  id,
  start,
  end,
  freq = "daily",
  benchmark = NULL,
  periodicity = "M",
  window = "Y",
  session
)
}
\arguments{
\item{containerType}{The endpoint to query, i.e "portfolios", "shareclasses".}

\item{id}{The container id.}

\item{start}{The start date of the desired performance.}

\item{end}{The end date of the desired performance.}

\item{freq}{The frequency of the observations. Default is 'daily'.}

\item{benchmark}{The benchmark id. Default is NULL.}

\item{periodicity}{The desired alternative periodicity of returns. 'M' for monthly, 'Q' for quarterly.}

\item{window}{The time window of analysis.}

\item{session}{The rdecaf session.}
}
\value{
A list with container, benchmark and relative performance data.
}
\description{
This is the description
}
