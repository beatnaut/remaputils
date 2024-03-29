% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health.R
\name{subsetFromDecaf}
\alias{subsetFromDecaf}
\title{This function subsets data from a decaf endpoint and styles it for email ready use.}
\usage{
subsetFromDecaf(
  endpnt,
  session,
  prams = "",
  func = NULL,
  failSafe = data.frame(`No Data` = character(), stringsAsFactors = FALSE),
  cols = NULL,
  colNames = NULL,
  omitCFlag = NULL,
  addLink = TRUE,
  Filter = TRUE
)
}
\arguments{
\item{endpnt}{string for the decaf endpoint, e.g. trades.}

\item{session}{the rdecaf session.}

\item{prams}{string containing the expression for the params to prefilter data in addParams of getDBObject. Defaults to NULL.}

\item{func}{list containing the function and its corresponding parameters to be applied where applicable. Defaults to NULL.}

\item{failSafe}{data frame to return in short circuits. Can be NULL, defaults to empty data frame showing 'no records'.}

\item{cols}{vector of the column names to keep - besides mandatory ID - in the returned DF. Defaults to NULL.}

\item{colNames}{vector of the stylized column names to use for the returned columns. Defaults NULL, in which case the raw names from above are used.}

\item{omitCFlag}{numeric indicating what cflag to omit if we are to apply omitRecordsByFlag fn. Defaults to NULL.}

\item{addLink}{logical indicating whether to add the decaf link. Defaults to TRUE, requires that the endpoint data frame have an 'id' column.}

\item{Filter}{logical indicating whether to filter the data on the condition column from the optional applied func. Defaults to TRUE.}
}
\value{
The endpoint data-frame with dubious data.
}
\description{
This is a description
}
