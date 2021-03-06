% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HegyiCICalculator.R
\docType{methods}
\name{HegyiCICalculator}
\alias{HegyiCICalculator}
\title{the function to calculate intraspecific and interspecific hegyi competition index both distance and size}
\usage{
HegyiCICalculator(
  objectID,
  species,
  coordX,
  coordY,
  size,
  maxRadius,
  distanceWeight = 1,
  sizeWeight = 0
)
}
\arguments{
\item{objectID}{character, The unique object identifier. Must be unique.}

\item{species}{character, Species code to identify intra and inter-specific competition.}

\item{coordX}{numeric, The x coordinate.}

\item{coordY}{numeric, The y coordinate.}

\item{size}{numeric, The size that used for compute competition index.}

\item{maxRadius}{numeric, The competition index will been calculated within this radius}

\item{distanceWeight}{numeric, Define how the compeition sensitive to the distance of a neighbours, ie., crowdness.
Default is 1, which is same as the original Hegyi index.}

\item{sizeWeight}{numeric, Define how the compeition scales across all the plots. Default is 0, which means there is no scale.}
}
\value{
a data table that has five columns, plotNumber, treeNumber, Year, IntraH and InterH
}
\description{
the function to calculate intraspecific and interspecific hegyi competition index both distance and size
}
\note{
no note
}
\seealso{
no
}
\author{
Yong Luo
}
