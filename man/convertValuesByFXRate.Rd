% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{convertValuesByFXRate}
\alias{convertValuesByFXRate}
\title{This is function converts values to desired currency.}
\usage{
convertValuesByFXRate(
  df,
  ccyFld = "ccy",
  dtFld = "date",
  valFld = c("nav", "aum"),
  convertTo = "USD",
  session
)
}
\arguments{
\item{df}{The data frame.}

\item{ccyFld}{The column name with the 3 letter currency information.}

\item{dtFld}{The column name with the date information.}

\item{valFld}{A vector with the name of the numeric columns to be converted.}

\item{convertTo}{The 3 letter currency to be converted to.}

\item{session}{The rdecaf session.}
}
\value{
The data frame with additional columns.
}
\description{
This is the description
}
