% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/areaProportion.R
\docType{methods}
\name{areaProportion}
\alias{areaProportion}
\title{This function is to derive a correction index to account for edge effect when account for
competition effect}
\usage{
areaProportion(
  bearing,
  distance,
  radius,
  baseShape = "circle",
  baseRadius = 10,
  baseCorners = list(c(-50, 50), c(50, 50), c(50, -50), c(-50, -50))
)
}
\arguments{
\item{bearing}{numeric, The bearing of a tree from a given point of plot (centre or corner).}

\item{distance}{numeric, The distance of a tree from a given point of plot (centre or corner).}

\item{radius}{numeric, The radius for a focal subject, which define the circular area around
a subject tree.}

\item{baseShape}{character, The shape of the area to be overlapped. Must be either
\code{circle} or \code{rectangle}. Default is \code{circle}.}

\item{baseRadius}{numeric, The radius for the base area, if the shape is defined as \code{circle}.
Default is 10.}

\item{baseCorners}{list, If the shape is defined as \code{rectangle}, this argument specifies
upper left, upper right, lower right and lower left corners.
Default is \code{list(c(-50, 50), c(50, 50), c(50, -50), c(-50, -50))},
which represent a 10000 m2 base area.}
}
\value{
A ratio of overlapped area to full circular area.
}
\description{
The correction index is calculated using proportion of overlapped area
to full circular area.
}
\examples{
\dontrun{
# given a tree is located with bearing of 150 degree and distance of 13 m
# and in a plot of 16.9 m radius circle, in which all the trees are measured.
# assume all the trees within 10m radius have competitive effect on this tree
# hence the trees that are in the plot and within 10m radius from focal tree
# should be corrected based on proportion
# to calculate the proportion
proportion <- areaProportion(bearing = 150,
                             distance = 13,
                             radius = 10,
                             baseShape = "circle",
                             baseRadius = 16.9)



}

}
\author{
Yong Luo
}
