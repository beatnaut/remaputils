% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analytics.R
\name{getHistoricalUserActions}
\alias{getHistoricalUserActions}
\title{This function gets the historical user actions in a deployment.}
\usage{
getHistoricalUserActions(
  session,
  endpoint,
  sample = 1000,
  exclCols = NULL,
  exclKeys = NULL,
  asof = NULL
)
}
\arguments{
\item{session}{The rdecaf session.}

\item{endpoint}{The endpoint to investigate, i.e "trades", "quants".}

\item{sample}{The pagesize row length.}

\item{exclCols}{The columns to exclude.}

\item{exclKeys}{The keys to exclude from exclCols.}

\item{asof}{Addtional query parameters to be passed to params.}
}
\value{
A table with the automation rate.
}
\description{
This function gets the historical user actions in a deployment.
}
