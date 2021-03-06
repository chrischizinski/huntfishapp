% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_functions.R
\name{countItems}
\alias{countItems}
\title{Count items by group}
\usage{
countItems(df, groupVars = NULL)
}
\arguments{
\item{df}{A data frame}

\item{groupVars}{A character vector of variable names to group by}
}
\value{
A data frame with columns for grouping variables and a column named
\code{items} for number of items. Data frame is passed through
\code{\link{prettyData}} function.
}
\description{
Count number of items (permits, licenses, stamps, etc.) by group. This
function will collect data from the database if using SQL backend.
}
\examples{
# Demo data: Count number of deer licenses each year between 2010 and 2017
filterData(
  dataSource = "csv",
  activeFilters = list(itemType = "Deer", itemYear = c(2010, 2017))
) \%>\%
  countItems(c("itemYear", "itemType"))
\dontrun{
# Database connection. Suggest using keyring package to avoid hardcoding
# passwords
myConn <- DBI::dbConnect(odbc::odbc(),
  dsn = "HuntFishApp", # Your datasource name
  uid = keyring::key_get("HuntFishAppUID"), # Your username
  pwd = keyring::key_get("HuntFishAppPWD")
) # Your password

# SQL Backend: Count number of deer licenses each year between 2010 and 2017
filterData(
  dataSource = "sql",
  conn = myConn,
  activeFilters = list(itemType = "Deer", itemYear = c(2010, 2017))
) \%>\%
  countItems(c("itemYear", "itemType"))
}

}
\seealso{
Other analysis functions: \code{\link{calcChurn}},
  \code{\link{calcGenderProportion}},
  \code{\link{calcParticipation}},
  \code{\link{calcRecruitment}},
  \code{\link{countCustomers}},
  \code{\link{itemGroupCount}}, \code{\link{sumRevenue}}
}
\concept{analysis functions}
