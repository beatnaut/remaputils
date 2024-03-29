% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/holdings.R
\name{getEnrichedHoldings}
\alias{getEnrichedHoldings}
\title{A function to get the enriched holdings data-frame.}
\usage{
getEnrichedHoldings(
  holdings,
  nav,
  gav,
  regions,
  resources,
  addTagsBy = NULL,
  childDefaults = TRUE
)
}
\arguments{
\item{holdings}{The holdings data-frame as returned by getFlatHoldings}

\item{nav}{The nav as returned by getResource("consolidation").}

\item{gav}{The gav as returned by getResource("consolidation").}

\item{regions}{The data-frame with the country to region mapping.}

\item{resources}{The data-frame as returned by getResource("resources")}

\item{addTagsBy}{TODO.}

\item{childDefaults}{TODO.}
}
\value{
A data-frame with the enriched holdings.
}
\description{
This is the description
}
