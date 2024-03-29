% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{smoothPlot}
\alias{smoothPlot}
\title{This function plots a linear regression plot.
It assumes there is an x value that explains a y value (e.g. benchmark values).}
\usage{
smoothPlot(df, xval, yval, col, scol = "#444341", ttl)
}
\arguments{
\item{df}{the data frame containing the x and y values.}

\item{xval}{the x value as a string.}

\item{yval}{the y value as a string.}

\item{col}{the color of the ponts (usually gradient using centeredColorScale fn).}

\item{scol}{the smoothing shade color, defaults to grey.}

\item{ttl}{the title given to the plot as a string.}
}
\value{
A LM plot.
}
\description{
This is the description
}
