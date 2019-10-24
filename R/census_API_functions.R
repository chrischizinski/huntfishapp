
#' Census API function
#'
#' Query data from US Census Bureau API and collect result into dplyr data frame
#'
#'
#' For information on the US Census API see \url{https://www.census.gov/developers/}.
#'
#' This function is a modified version of the code published here:
#' \url{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}
#'
#' @param data_url The url root of the api including the '?'
#' @param key API key
#' @param vars A character vector of variable names
#' @param region A region specification containing a 'for:' and possibly an 'in:'
#'
#' @family census functions
#'
#' @examples
#' \donttest{
#' runCensusAPIQueries(
#'   data_url = "https://api.census.gov/data/2017/acs/acs5?",
#'   key = rstudioapi::askForSecret("censusKey"),
#'   vars = paste0("B01001_", str_pad(2:49, 3, pad = "0"), "E"),
#'   region = "for=county:*&in=state:31"
#' )
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
runCensusAPIQueries <- function(data_url, key, vars, region, numeric = TRUE) {

  # If more than 50 variables, split into multiple queries
  if (length(vars) > 50) {

    # Collapse variables
    vars <- vecToChunk(vars)
    get <- lapply(vars, function(x)
      paste(x, sep = "", collapse = ","))

    # Loop over queries
    queryResult <-
      lapply(get, function(x)
        queryCensusAPI(
          data_url = data_url,
          key = key,
          get = x,
          region = region,
          numeric = numeric
        ))
  } else {

    # Collapse variables
    get <- paste(vars, sep = "", collapse = ",")

    # Execute query
    queryResult <-
      list(queryCensusAPI(data_url, key, get, region, numeric = TRUE))
  }

  # Format output.  If there were no errors, than paste the queryResult together
  # If there is an error, just return the unformatted list.
  if (all(sapply(queryResult, is.data.frame))) {
    queryVars <- unlist(lapply(queryResult, names))
    queryResult <- do.call(cbind, queryResult)
    names(queryResult) <- queryVars

    # Drop duplicate columns (e.g., geography variables)
    queryResult <- queryResult[unique(colnames(queryResult))]

    return(as_tibble(queryResult))
  } else {
    warning("Unable to merge results into single data frame.")
    return(queryResult)
  }
}

#' Census query function
#'
#' Pull data from US Census API
#'
#'
#' For information on the US Census API see \url{https://www.census.gov/developers/}.
#'
#' This function is a modified version of the code published here:
#' \url{http://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html}
#'
#' @param data_url The url root of the api including the '?'
#' @param key API key
#' @param vars A character vector of variable names
#' @param region A region specification containing a 'for:' and possibly an 'in:'
#'
#' @family census functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
queryCensusAPI <- function(data_url, key, get, region, numeric = TRUE) {
  # if (length(get) > 1)
  #   get <- paste(get, collapse = ',', sep = '')

  # URL for API query
  api_call <- paste(data_url,
    "get=", get,
    "&", region,
    "&key=", key,
    sep = ""
  )

  # Try to execute query
  queryRaw <- try(readLines(api_call, warn = "F"))

  # Display error
  if (class(queryRaw) == "try-error") {
    warning(paste("Census API query failed:", api_call))
    return()
  }

  # Split data into a list with each row as an element
  tmp <- strsplit(gsub("[^[:alnum:], _]", "", queryRaw), "\\,")

  # Convert results to data frame
  queryResult <- as.data.frame(do.call(rbind, tmp[-1]),
    stringsAsFactors = FALSE
  )
  names(queryResult) <- tmp[[1]]

  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  #  Example: field names of B01001_001E vs state
  if (numeric == TRUE) {
    value_cols <- grep("[0-9]", names(queryResult), value = TRUE)
    for (col in value_cols) {
      queryResult[, col] <- as.numeric(as.character(queryResult[, col]))
    }
  }
  return(queryResult)
}

#' Split vector into chunks
#'
#' Helper function to split Census API queries into chunks
#'
#' @param x A vector
#' @param max Size of chunks
#'
#' @family census functions
#'
vecToChunk <- function(x, max = 50) {
  s <- seq_along(x)
  x1 <- split(x, ceiling(s / max))
  return(x1)
}
