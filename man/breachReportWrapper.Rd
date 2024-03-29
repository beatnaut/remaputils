% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compliance.R
\name{breachReportWrapper}
\alias{breachReportWrapper}
\title{The wrapper function for the breach report which emails the report.}
\usage{
breachReportWrapper(
  portfolios = NULL,
  tz = "UTC",
  gte = "00:00:00",
  lte = "23:59:59",
  weekdays = c("MON", "TUE", "WED", "THU", "FRI"),
  session,
  emailParams,
  rDate = Sys.Date(),
  field = "issuer",
  divisor = "nav",
  minLim = 0,
  maxLim = 0.3,
  excludeCtypes = NULL,
  emailList = NULL
)
}
\arguments{
\item{portfolios}{The portfolio id's. Default is NULL which runs all portfolio id's.}

\item{tz}{The time zone.}

\item{gte}{The lower time limit for the report to run.}

\item{lte}{The upper time limit for the report to run.}

\item{weekdays}{The weekdays when the report should be run.}

\item{session}{The session info.}

\item{emailParams}{The email parameters.}

\item{rDate}{The date of the report. Default is Sys.Date.}

\item{field}{The resources field. i.e issuer for issuer concentration or id for single concentration.}

\item{divisor}{The consolidation list name to be used as the divisor. Default is nav.}

\item{minLim}{The lower limit.}

\item{maxLim}{The upper limit.}

\item{excludeCtypes}{A vector with strings, i.e CCY or NULL. Default is NULL.}

\item{emailList}{Optionally, the email list. Default is NULL.}
}
\value{
NULL.
}
\description{
This is a description.
}
