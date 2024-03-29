% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfra.R
\name{cfraDetails}
\alias{cfraDetails}
\title{CFRA-pocketbase dependent function to gather the CFRA details/appendix info.}
\usage{
cfraDetails(
  tick,
  exch,
  lastStarDate = NA,
  lastHgltDate = NA,
  path2Config = "cfra/cfraAuth.R"
)
}
\arguments{
\item{tick}{The ticker that identifies the instrument in CFRA DB, e.g. 'AAPL'}

\item{exch}{The exchange that corresponds to the ticker in CFRA DB, e.g. 'NasdaqGS'}

\item{lastStarDate}{The date floor to supply for the star rank elements, for efficiency, e.g. 2023-01-01}

\item{lastHgltDate}{The date floor to supply for the star highlights elements, for efficiency, e.g. 2023-01-01}

\item{path2Config}{The path to the file containing the cfra configurations}
}
\value{
a list containing 3 data frame elements corresponding to stars ranking, research, and estimates data.
}
\description{
This is the description
}
