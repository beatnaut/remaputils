% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trades.R
\name{getContainerWiseTrades}
\alias{getContainerWiseTrades}
\title{A function to get account-wise trades from a DECAF instance.}
\usage{
getContainerWiseTrades(ids, containerType = "accounts", session, gte = NULL)
}
\arguments{
\item{ids}{A vector with container id's.}

\item{containerType}{The container type.}

\item{session}{The DECAF session info.}

\item{gte}{The date after which trades should be considered.}
}
\value{
A data-frame with DECAF trades.
}
\description{
This is the description
}
