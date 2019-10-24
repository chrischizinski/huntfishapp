#' State population
#'
#' A dataset containing population by gender, age group, and county
#' based on the US Census Bureau American Community Survey 5-Year Data (2009-2017)
#' \url{https://www.census.gov/programs-surveys/acs/}.
#'
#' The ACS population estimates for smaller bins were added to obtain the
#' following age bins:
#' \itemize{
#'   \item 0-17
#'   \item 18-24
#'   \item 25-34
#'   \item 35-44
#'   \item 45-54
#'   \item 55-64
#'   \item 65+
#'   }
#'
#' Variables include
#' \itemize{
#'   \item State FIPS code (2 digit): \code{stateFP}
#'   \item County FIPS code (3 digit): \code{countyFP}
#'   \item Gender: \code{gender}
#'   \item Age group: \code{ageGroup}
#'   \item State abbreviation: \code{state}
#'   \item County name: \code{county}
#' }
#'
#' @family census functions
#'
#' @source \url{https://www.census.gov/data/developers/data-sets/acs-5year.html}
"statePop"
