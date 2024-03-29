% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marketStack.R
\name{mstackData}
\alias{mstackData}
\title{Returns df of time series from marketstack}
\usage{
mstackData(
  timeOfDay = "eod",
  mstackAPIKey,
  mstackSymbol,
  mstackSort = "DESC",
  mstackStart = Sys.Date() - 7,
  mstackEnd = Sys.Date(),
  mstackLimit = 1000
)
}
\arguments{
\item{timeOfDay}{e.g. eod or intraday (15 min int, avoid/unreliable)}

\item{mstackAPIKey}{marketstack API key}

\item{mstackSymbol}{ticker per marketstack, e.g. RUT.INDX = Russell 2000}

\item{mstackSort}{default = DESC}

\item{mstackStart}{starting date, fmt = YYYY-MM-DD, defaults to current date less a week}

\item{mstackEnd}{ending date, fmt = YYYY-MM-DD, defaults to current date}

\item{mstackLimit}{limit on records returned, defaults to 1000 (max allowed)}
}
\value{
A data frame with market data.
}
\description{
This is a description.
}
