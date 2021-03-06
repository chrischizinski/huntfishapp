% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_interactions.R
\name{createRadialsetsTooltip}
\alias{createRadialsetsTooltip}
\title{Tooltip for radial sets plot}
\usage{
createRadialsetsTooltip(sharedCustomers, degreeCount, hover,
  focusGroup = "none", linkThickness = "percent",
  countScale = 1/1000)
}
\arguments{
\item{hover}{A named list created by \code{\link[shiny]{plotOutput}}}

\item{plotData}{A data frame with data used in plot}
}
\value{
A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
}
\description{
Build tooltip for radial sets network plot
}
