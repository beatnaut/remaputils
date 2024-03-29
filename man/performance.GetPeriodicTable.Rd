% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{performance.GetPeriodicTable}
\alias{performance.GetPeriodicTable}
\title{A function to generate periodic return data for a given window.}
\usage{
performance.GetPeriodicTable(period, returns, window)
}
\arguments{
\item{period}{The period of analysis, i.e 'W', 'M', Q', 'Y'.}

\item{returns}{a xts vector with returns.}

\item{window}{The window for analysis, i.e 'W', 'M', Q', 'Y'.}
}
\value{
A list with periodic returns, current window returns and historical window returns.
}
\description{
This is the description
}
