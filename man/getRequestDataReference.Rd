% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bbg.R
\name{getRequestDataReference}
\alias{getRequestDataReference}
\title{A function to build the requestable currency pairs and the entire combinations of currencies}
\usage{
getRequestDataReference(
  session,
  date,
  zero,
  underlying,
  bbgFields,
  resources,
  byStock = TRUE,
  forceAux = FALSE,
  exclCtypes = c("CCY", "LOAN", "FXFWD", "DEPO")
)
}
\arguments{
\item{session}{The rdecaf session.}

\item{date}{The date.}

\item{zero}{The zero parameter for stocks.}

\item{underlying}{Shall underlying be included if stocks are to be considered?}

\item{bbgFields}{The list of bbg field mappings.}

\item{resources}{The entire resource data frame.}

\item{byStock}{Should the request data be based on stocks only?}

\item{forceAux}{Are the aux fields to be forced?}

\item{exclCtypes}{Which ctypes should be excluded from consideration?}
}
\value{
A list with the reference request data objest and the resources.
}
\description{
A function to build the requestable currency pairs and the entire combinations of currencies
}
