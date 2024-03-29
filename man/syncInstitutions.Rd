% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync.R
\name{syncInstitutions}
\alias{syncInstitutions}
\title{A function to sync institutions between 2 DECAF instances:}
\usage{
syncInstitutions(sourceSession, targetSession, sourceAccounts, trades = NULL)
}
\arguments{
\item{sourceSession}{The session info for the DECAF instance to sync from.}

\item{targetSession}{The session info for the DECAF instance to sync to.}

\item{sourceAccounts}{The accounts data-frame coming from the source instance.}

\item{trades}{If trades is given, additional institutions based on feeagt will be created. Default is NULL.}
}
\value{
The function creates the institutions in the target instance and returns a institutions data-frame.
}
\description{
This is the description
}
