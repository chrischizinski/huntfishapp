% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditional_dplyr.R
\name{conditional_mutate}
\alias{conditional_mutate}
\title{Conditional mutate function}
\usage{
conditional_mutate(df1, cond, LHS, RHS)
}
\arguments{
\item{df1}{A dataframe}

\item{cond}{A logical value indicating if mutate should be performed}

\item{varName}{Name of variable to create}
}
\description{
Perform mutate if condition is met. This function is a simple wrapper for
\code{\link[dplyr]{mutate}}.
}
\examples{
mtcars \%>\%
  dplyr::as_tibble() \%>\%
  conditional_mutate(
    exists("cyl", where = .),
    LHS = cyl2,
    RHS = cyl * 2
  )

mtcars \%>\%
  dplyr::as_tibble() \%>\%
  conditional_mutate(
    exists("cyl2", where = .),
    LHS = cyl4,
    RHS = cyl2 * 2
  )
}
\seealso{
Other conditional dplyr: \code{\link{conditional_filter}},
  \code{\link{conditional_range_filter}},
  \code{\link{conditional_select}}
}
\concept{conditional dplyr}
