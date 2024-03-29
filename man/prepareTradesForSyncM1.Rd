% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{prepareTradesForSyncM1}
\alias{prepareTradesForSyncM1}
\title{Pushes vouchers in batches.}
\usage{
prepareTradesForSyncM1(accounts, sessionS, sessionT, resources, gte)
}
\arguments{
\item{accounts}{The accounts data frame.}

\item{sessionS}{The rdecaf session at source.}

\item{sessionT}{The rdecaf session at target.}

\item{resources}{The resources data frame.}

\item{gte}{The greater than date parameter.}
}
\value{
A data frame with the prepared trades.
}
\description{
This is the description
}
