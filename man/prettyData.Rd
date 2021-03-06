% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_functions.R
\name{prettyData}
\alias{prettyData}
\title{Pretty data function}
\usage{
prettyData(df)
}
\arguments{
\item{df}{A dataframe}
}
\description{
Prepare data for plotting.
}
\details{
This function performs the following operations (if the variables exist):
\itemize{
\item \code{itemYear} is converted to factor format
\item \code{ageGroup} is converted to factor format with levels \code{c("0-17", "18-24",
"25-34", "35-44", "45-54", "55-64", "65+", "Invalid")}
\item \code{issueDate} is converted to date format
\item \code{monthAbb} factor variable is created with three-letter abbreviations for
the English month names
\item \code{yearAbb} factor variable is created with two-digit year abbreviations
\item \code{monthYear} factor variable is created with three-letter month
abbreviations and two-digit year abbreviations
\item \code{residency} is converted to a factor with levels \code{c("Resident",
"Non-Resident")}
\item \code{itemResidency} is converted to a factor with levels \code{c("Resident Item",
"Non-Resident Item")}
\item \code{gender} is converted to a factor
\item \code{itemType} is converted to a factor
\item Any factor variables with missing values are given an explicit factor
level to ensure that they appear in summaries and on plots
}
}
