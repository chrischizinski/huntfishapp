#' Load and filter data
#'
#' This function has the option to either load data from a local file,
#' \code{dataSource = "csv"}, or a database backend, \code{dataSource = "sql"}.
#' If using a local file, a data frame is returned. If using a database backend,
#' an unevaluated \code{dplyr} data frame corresponding to a SQL query is returned.
#'
#' When \code{dataSource = "csv"}, the function will return data from the built
#' in dataset \code{data("huntfishappDemo", package = "huntfishapp")}. The
#' option to use a local file is provided mainly to demonstrate the app functionality
#' if a database backend is unavailabe. To use a different data file the source
#' code of the function will need to be modified. Using a local data file has
#' the advantage that analysis can occur very quickly because all the data is
#' loaded into memory. The drawbacks to using a local data file are that the
#' amount of data that  can be analyzed is limited by the memory available, it
#' may not scale well if the app is deployed to multiple users, and it may be
#' tedious to manage and update local data files.
#'
#' The suggested use case is \code{dataSource = "sql"} to use a database backend.
#' When using a database backend the function will attempt to connect to a view
#' on the server named \code{"huntfishapp"} using the user supplied connection object,
#' \code{tbl(conn, "huntfishapp")}. The view should contain the following variables:
#' \itemize{
#'   \item{itemUID: Unique integer identifier for each item (i.e., row).}
#'   \item{customerUID: Unique integer identifier for customer who purchased item.}
#'   \item{issueDate: Date item was purchased or issued in the format yyyy-mm-dd.}
#'   \item{itemResidency: Residency attribute of item coded as "T" or "F". May
#'   or may not correspond to customer residency attribute.}
#'   \item{duration: Duration attribute of item in character format. Any values
#'   are allowed.}
#'   \item{residency: Residency attribute of customer coded as "T" or "F". May
#'   or may not correspond to item residency attribute. May or may not change
#'   over time.}
#'   \item{state: Customer address state component coded as two letter abbreviation.}
#'   \item{county: Customer address county component in title-case.}
#'   \item{age: Customer age attribute.}
#'   \item{itemType: Description of item type (e.g., Deer, Fish, etc.)}
#'   \item{itemYear: Year attribute of item. May or may not correspond to year of
#'   purchase or issue date.}
#'   \item{price: Price of item including relevant fees.}
#' }
#'
#' @param dataSource A string specifying whether to use database backend, \code{"sql"},
#' or a csv file, \code{"csv"}.
#' @param conn An object created with \code{\link[DBI]{dbConnect()}} specifying
#' a connection to a database management system. Only needed when
#' \code{dataSource = "sql"}.
#' @param activeFilters A list of elements with names corresponding to
#' variables and values indicating filter condition (see details)
#'
#' @return If \code{dataSource="sql"}, returns an unevaluated dplyr data frame
#' corresponding to a SQL query (class: tbl_dbi,' tbl_sql, tbl_lazy, tbl). If
#' \code{dataSource="csv"}, returns a standard dplyr data frame.
#'
#' @examples
#' # Demo data: All items for male customers age 0-17 in Lancaster county
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(
#'     gender = "Male",
#'     county = "lancaster",
#'     ageGroup = "0-17"
#'   )
#' )
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Collect 10 resident waterfowl and hunt items for January 2017
#' wfJan2017 <-
#'   filterData(
#'     dataSource = "sql",
#'     conn = myConn,
#'     activeFilters = list(
#'       issueDate = c("2017-01-01", "2017-01-31"),
#'       itemType = c("Waterfowl", "Hunt"),
#'       residency = "T"
#'     )
#'   )
#'
#' # Inspect SQL query (useful for debugging)
#' wfJan2017 %>% show_query()
#'
#' # Evaluate SQL query (set n = Inf to return all rows)
#' wfJan2017 %>% collect(n = 10)
#' }
#'
#' @import dplyr
#'
#' @export
filterData <-
  function(dataSource = "sql",
             conn = NULL,
             activeFilters = NULL) {

    # Check inputs ----------------------------------------------------------

    # Check activeFilters class
    if (!(is.null(activeFilters) | class(activeFilters) == "list")) {
      stop("Input 'activeFilters' should be a list.")
    }

    # Extract names of filter variables
    filterVars <- names(activeFilters)

    # List of valid filter variables
    validFilterVars <- c(
      "issueDate", "itemResidency", "duration", "residency",
      "state", "county", "gender", "age", "itemType", "itemYear",
      "ageGroup", "month", "year"
    )

    # Check that filter variables are valid
    if (!all(filterVars %in% validFilterVars)) {
      stop(
        paste0(
          "Unrecognized filter variables: ",
          paste(filterVars[!(filterVars %in% validFilterVars)], collapse = ", ")
        ),
        ". ",
        "Valid filter variables are: ",
        paste(validFilterVars, collapse = ", ")
      )
    }

    # Load data from database or flat file
    if (dataSource == "sql") {
      # Check conn class
      if (class(conn)[1] != "Microsoft SQL Server") {
        stop(paste("Input 'conn' should be Microsoft SQL Server connection."))
      }
      # Try to connect to database
      result <- tryCatch({
        tbl(conn, "huntfishapp") %>%
          collect(n = 1)
      }, warning = function(w) {
        stop("The view 'huntfishapp' does not exist or user does not have the necessary privleges.")
      }, error = function(e) {
        stop("The view 'huntfishapp' does not exist or user does not have the necessary privleges.")
      })
      licenseData <- tbl(conn, "huntfishapp")
    } else {
      data("huntfishappDemo", package = "huntfishapp")
      licenseData <- huntfishappDemo
    }

    # List of required variables
    reqVars <- c(
      "itemUID", "customerUID", "issueDate", "itemResidency", "duration",
      "residency", "state", "county", "gender", "age", "itemType",
      "itemYear", "price"
    )

    # Check that filter variables are valid
    if (!all(reqVars %in% colnames(licenseData))) {
      stop(
        paste0(
          "Missing required variable(s): ",
          paste(reqVars[!(reqVars %in% colnames(licenseData))], collapse = ", ")
        )
      )
    }

    # Filter data
    licenseData %>%
      mutate(
        ageGroup = case_when(
          age <= -1 ~ "Invalid",
          age <= 17 ~ "0-17",
          age <= 24 ~ "18-24",
          age <= 34 ~ "25-34",
          age <= 44 ~ "35-44",
          age <= 54 ~ "45-54",
          age <= 64 ~ "55-64",
          age <= 120 ~ "65+",
          TRUE ~ "Invalid"
        )
      ) %>%
      mutate(month = month(issueDate)) %>%
      mutate(year = year(issueDate)) %>%
      conditional_filter(
        cond = "gender" %in% filterVars,
        varName = "gender",
        varCond = activeFilters[["gender"]]
      ) %>%
      conditional_filter(
        cond = any(c("itemType") %in% filterVars),
        varName = "itemType",
        varCond = activeFilters[["itemType"]]
      ) %>%
      conditional_filter(
        cond = "duration" %in% filterVars,
        varName = "duration",
        varCond = activeFilters[["duration"]]
      ) %>%
      conditional_filter(
        cond = "state" %in% filterVars,
        varName = "state",
        varCond = activeFilters[["state"]]
      ) %>%
      conditional_filter(
        cond = "county" %in% filterVars,
        varName = "county",
        varCond = tolower(activeFilters[["county"]])
      ) %>%
      conditional_filter(
        cond = "residency" %in% filterVars,
        varName = "residency",
        varCond = activeFilters[["residency"]]
      ) %>%
      conditional_filter(
        cond = "itemResidency" %in% filterVars,
        varName = "itemResidency",
        varCond = activeFilters[["itemResidency"]]
      ) %>%
      conditional_range_filter(
        cond = "itemYear" %in% filterVars,
        varName = "itemYear",
        varCond = activeFilters[["itemYear"]]
      ) %>%
      conditional_range_filter(
        cond = "age" %in% filterVars,
        varName = "age",
        varCond = activeFilters[["age"]]
      ) %>%
      conditional_filter(
        cond = "ageGroup" %in% filterVars,
        varName = "ageGroup",
        varCond = activeFilters[["ageGroup"]]
      ) %>%
      conditional_range_filter(
        cond = "month" %in% filterVars,
        varName = "month",
        varCond = activeFilters[["month"]]
      ) %>%
      conditional_range_filter(
        cond = "year" %in% filterVars,
        varName = "year",
        varCond = activeFilters[["year"]]
      ) %>%
      conditional_range_filter(
        cond = "issueDate" %in% filterVars,
        varName = "issueDate",
        varCond = activeFilters[["issueDate"]]
      )
  }
