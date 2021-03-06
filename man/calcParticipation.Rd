% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_functions.R
\name{calcParticipation}
\alias{calcParticipation}
\title{Calculate participation rate}
\usage{
calcParticipation(df, groupVars, pop)
}
\arguments{
\item{df}{A data frame}

\item{groupVars}{A character vector of variable names to group by}

\item{pop}{A dataframe of population by county, year, gender, and age group.}
}
\description{
Calculate participation rate by group. Participation rate is the number of
customers divided by the population size.
}
\examples{
# Get list of valid counties (used to filter out data entry errors)
validCounties <- countyMaps \%>\%
  dplyr::filter(region == "nebraska") \%>\%
  dplyr::distinct(county) \%>\%
  dplyr::pull(county)

# Demo data: Participation rate for customers purchasing a fishing
# license between 2010 and 2017
filterData(
  dataSource = "csv",
  activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017), state = "NE")
) \%>\%
  calcParticipation(c("itemType", "county"),
    pop = statePop \%>\% filter(state == "NE")
  ) \%>\%
  dplyr::filter(county \%in\% validCounties)
\dontrun{
# Database connection. Suggest using keyring package to avoid hardcoding
# passwords
myConn <- DBI::dbConnect(odbc::odbc(),
  dsn = "HuntFishApp", # Your datasource name
  uid = keyring::key_get("HuntFishAppUID"), # Your username
  pwd = keyring::key_get("HuntFishAppPWD")
) # Your password

# Get list of valid counties (used to filter out data entry errors)
validCounties <- countyMaps \%>\%
  dplyr::filter(region == "nebraska") \%>\%
  dplyr::distinct(county) \%>\%
  dplyr::pull(county)

# SQL Backend: Participation rate for customers purchasing a fishing
# license between 2010 and 2017
filterData(
  dataSource = "sql",
  conn = myConn,
  activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017), state = "NE")
) \%>\%
  calcParticipation(c("itemType", "county"),
    pop = statePop \%>\% filter(state == "NE")
  ) \%>\%
  dplyr::filter(county \%in\% validCounties)
}

}
\seealso{
Other analysis functions: \code{\link{calcChurn}},
  \code{\link{calcGenderProportion}},
  \code{\link{calcRecruitment}},
  \code{\link{countCustomers}}, \code{\link{countItems}},
  \code{\link{itemGroupCount}}, \code{\link{sumRevenue}}
}
\concept{analysis functions}
