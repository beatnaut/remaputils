% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{messageHandler}
\alias{messageHandler}
\title{This function handles the log messages}
\usage{
messageHandler(
  x = 1,
  val = 0,
  condition = "equal",
  errortext = NULL,
  infotext = NULL,
  warntext = NULL,
  logger = "",
  addlocation = NULL
)
}
\arguments{
\item{x}{The result value of a condition. Default 1.}

\item{val}{The value to check against. Default 0.}

\item{condition}{The condition to check for. Either "equal", "smaller" or "greater". Default is equal.}

\item{errortext}{The text to be included in the error message. Default is NULL.}

\item{infotext}{The info text to be included in the success case. Default is NULL.}

\item{warntext}{The warning text to be included in the dubious success case. Default is NULL.}

\item{logger}{The logger parameter for logwarn, logerror, loginfo. Default is "".}

\item{addlocation}{TODO:}
}
\value{
A vector of numerical values.
}
\description{
This is a description
}
