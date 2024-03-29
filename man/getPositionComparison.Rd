% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/positionIncrement.R
\name{getPositionComparison}
\alias{getPositionComparison}
\title{This function masks decaf positions based on provider positions:}
\usage{
getPositionComparison(pPos, resources, pDate, type, session)
}
\arguments{
\item{pPos}{The positions data frame from the provider.}

\item{resources}{The resources data frame}

\item{pDate}{The position date.}

\item{type}{The positions type. Either 'Security' or 'Cash' or 'FX Forward'.}

\item{session}{The rdecaf session.}
}
\value{
Returns the side by side comparison of decaf position and provider position.
}
\description{
This is the description
}
