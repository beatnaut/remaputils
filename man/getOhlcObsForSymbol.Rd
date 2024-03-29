% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ohlc.R
\name{getOhlcObsForSymbol}
\alias{getOhlcObsForSymbol}
\title{This function retrievs the ohlc observations for a give symbol.}
\usage{
getOhlcObsForSymbol(
  session,
  symbol,
  lte = Sys.Date(),
  lookBack = 30,
  excludeWeekends = TRUE,
  addFields = NULL
)
}
\arguments{
\item{session}{The session info for the instance to retrieve the ohlc observation from.}

\item{symbol}{The symbol to for which the ohlc observation is to be retrieved.}

\item{lte}{Less than or equal to this date. Default is Sys.Date(), meaning today.}

\item{lookBack}{The number of days to look back from 'lte'. Default is 30 days.}

\item{excludeWeekends}{Should the weekends be excluded? Default is TRUE.}

\item{addFields}{Additional fields to be queried.}
}
\value{
A data frame with the symbosl ohlc observation.
}
\description{
This is a description
}
