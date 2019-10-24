

#' Pretty data function
#'
#' Prepare data for plotting.
#'
#' This function performs the following operations (if the variables exist):
#' \itemize{
#' \item \code{itemYear} is converted to factor format
#' \item \code{ageGroup} is converted to factor format with levels \code{c("0-17", "18-24",
#' "25-34", "35-44", "45-54", "55-64", "65+", "Invalid")}
#' \item \code{issueDate} is converted to date format
#' \item \code{monthAbb} factor variable is created with three-letter abbreviations for
#' the English month names
#' \item \code{yearAbb} factor variable is created with two-digit year abbreviations
#' \item \code{monthYear} factor variable is created with three-letter month
#' abbreviations and two-digit year abbreviations
#' \item \code{residency} is converted to a factor with levels \code{c("Resident",
#' "Non-Resident")}
#' \item \code{itemResidency} is converted to a factor with levels \code{c("Resident Item",
#' "Non-Resident Item")}
#' \item \code{gender} is converted to a factor
#' \item \code{itemType} is converted to a factor
#' \item Any factor variables with missing values are given an explicit factor
#' level to ensure that they appear in summaries and on plots
#' }
#'
#' @param df A dataframe
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
prettyData <- function(df) {
  df %>%
    # Convert item year to factor
    conditional_mutate(
      exists("itemYear", where = .),
      LHS = itemYear, RHS = as.factor(itemYear)
    ) %>%
    # Convert age group to factor
    conditional_mutate(
      exists("ageGroup", where = .),
      LHS = ageGroup, RHS = factor(ageGroup,
        levels = c(
          "0-17", "18-24",
          "25-34", "35-44",
          "45-54", "55-64",
          "65+", "Invalid"
        )
      )
    ) %>%
    # Convert issue date to date
    conditional_mutate(
      exists("issueDate", where = .),
      LHS = issueDate, RHS = as.Date(issueDate)
    ) %>%
    # Create month abbreviation variable
    conditional_mutate(
      exists("month", where = .),
      LHS = monthAbb,
      RHS = factor(month.abb[month], levels = month.abb)
    ) %>%
    # Create year abbreviation variable
    conditional_mutate(
      exists("year", where = .),
      LHS = yearAbb,
      RHS = forcats::fct_reorder(substr(year, 3, 4), year)
    ) %>%
    # Create month year variable
    conditional_mutate(
      exists("month", where = .) & exists("year", where = .),
      LHS = monthYear,
      RHS = paste(monthAbb, yearAbb, sep = "-")
    ) %>%
    # Order levels of month year variable
    conditional_mutate(
      exists("month", where = .) & exists("year", where = .),
      LHS = monthYear,
      RHS = factor(monthYear, levels = unique(monthYear[order(yearAbb, monthAbb)]))
    ) %>%
    # Convert residency to descriptive values
    conditional_mutate(
      exists("residency", where = .),
      LHS = residency,
      RHS = forcats::fct_recode(residency,
        `Resident` = "T",
        `Non-resident` = "F"
      )
    ) %>%
    # Convert residency of item to descriptive values
    conditional_mutate(
      exists("itemResidency", where = .),
      LHS = itemResidency,
      RHS = forcats::fct_recode(
        itemResidency,
        `Resident Item` = "T",
        `Non-resident Item` = "F"
      )
    ) %>%
    # Convert gender to factor
    conditional_mutate(
      exists("gender", where = .),
      LHS = gender,
      RHS = as.factor(gender)
    ) %>%
    # Convert itemType to factor
    conditional_mutate(
      exists("itemType", where = .),
      LHS = itemType,
      RHS = as.factor(itemType)
    ) %>%
    # Replace missing values in factors
    mutate_if(is.factor, forcats::fct_explicit_na)
}

#' Round axis labels
#'
#' Scale axis labels
#'
#' @param df A data frame
#' @param y Name of y-variable to scale
#' @param groupVars Variables to group by
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
scaleVariable <- function(df, y, groupVars = NULL) {
  df %>%
    group_by(!!!syms(groupVars)) %>%
    mutate(!!sym(paste0(y, "Sum")) := sum(!!sym(y))) %>%
    ungroup() %>%
    mutate(divisor = case_when(
      (max(!!sym(paste0(y, "Sum"))) %/% 1e6) > 0 ~ 1e6,
      (max(!!sym(paste0(y, "Sum"))) %/% 1e3) > 0 ~ 1e3,
      TRUE ~ 1
    )) %>%
    select(-!!sym(paste0(y, "Sum"))) %>%
    mutate(!!sym(paste0(y, "Scaled")) := !!sym(y) / divisor)
}


#' Age cut
#'
#' Define cuts in age vector
#'
#' @param x A vector of ages
#' @param lower Lower bound for age bins
#' @param upper Upper bound for ang bins
#' @param by Size of age bins
#' @param sep Separator character for bin labels
#' @param above.char Character indicating top bin
#'
#'
#' @export
ageCut <- function(x, lower = 0, upper, by = 10,
                   sep = "-", above.char = "+") {

  # Force upper bound to match bin size
  if ((upper %% by) != 0) {
    upper <- upper + (upper %% by)
  }

  # Special case of size one bins
  if (by == 1) {
    lvls <- c(
      seq(lower, upper - by, by = by),
      paste(upper, above.char, sep = "")
    )
    myCuts <-
      factor(if_else(x >= upper, paste(upper, above.char, sep = ""), as.character(x)),
        levels = lvls
      )
  } else {
    lvls <- c(
      paste(
        seq(lower, upper - by, by = by),
        seq(lower + by - 1, upper - 1, by = by),
        sep = sep
      ),
      paste(upper, above.char, sep = "")
    )

    myCuts <-
      cut(
        floor(x),
        breaks = c(seq(lower, upper, by = by), Inf),
        right = FALSE,
        labels = lvls
      )
  }

  return(myCuts)
}

#' Count customers by group
#'
#' Count number of customers by group. This function will collect data from
#' the database if using SQL backend.
#'
#' @param df A data frame with a column named customerUID
#' @param groupVars A character vector of variable names to group by
#'
#' @return A data frame with columns for grouping variables and a column named
#' \code{customers} for number of customers Data frame is passed through
#' \code{\link{prettyData}} function.
#'
#' @examples
#' # Demo data: Count number of customers each year purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   countCustomers(c("itemYear", "itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Count number of customers each year purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   countCustomers(c("itemYear", "itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
countCustomers <- function(df, groupVars = NULL) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for required columns
  reqVars <- c("customerUID")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  df %>%
    group_by(!!!(syms(groupVars))) %>%
    summarize(customers = n_distinct(customerUID)) %>%
    ungroup() %>%
    collect(n = Inf) %>%
    arrange(!!!syms(groupVars)) %>%
    prettyData()
}

#' Average participants by group
#'
#' Calculate average value by group
#'
#' @param df A data frame
#' @param startYear An integer
#' @param endYear An integer
#' @param groupVars A vector
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
calcAvgValue <- function(df, var, startYear, endYear, groupVars = NULL) {
  df %>%
    group_by(!!!(syms(c("itemYear", groupVars)))) %>%
    summarize(!!sym(var) := sum(!!sym(var))) %>%
    ungroup() %>%
    mutate(itemYear = as.integer(as.character(itemYear))) %>%
    filter(itemYear >= startYear, itemYear <= endYear) %>%
    group_by(!!!(syms(groupVars))) %>%
    summarize(!!sym(paste0(var, "Avg")) := mean(!!sym(var))) %>%
    ungroup() %>%
    mutate(avgLab = paste0("average (", startYear, "-", endYear, ")"))
}


#' Calculate gender proportion
#'
#' Calculate gender proportion by group. This function collects data from
#' the database.
#'
#' @param df A data frame with a column named customerUID
#' @param groupVars A character vector of variable names to group by
#'
#' @return A dataframe with columns for grouping variables and a column named
#' \code{genderProportion} for gender proportion. Data frame is passed through
#' \code{\link{prettyData}} function.
#'
#' @examples
#' # Demo data: Gender proportions for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcGenderProportion(c("itemYear", "itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Gender proportions for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcGenderProportion(c("itemYear", "itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
calcGenderProportion <- function(df, groupVars = NULL) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for required columns
  reqVars <- c("customerUID", "gender")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check invalid grouping variables
  if ("gender" %in% groupVars) {
    stop("Gender is not a valid grouping variable")
  }

  countCustomers(df, c("gender", groupVars)) %>%
    group_by(!!!(syms(groupVars))) %>%
    mutate(genderProportion = customers / sum(customers)) %>%
    ungroup() %>%
    arrange(!!!syms(c("gender", groupVars))) %>%
    prettyData()
}

#' Sum revenue by group
#'
#' Calculate total revenue by group. Total revenue is the sum price variable.
#'
#' @param df A data frame
#' @param groupVars A character vector of variable names to group by
#'
#' @return A dataframe with columns for grouping variables and a column named
#' revenue for total revenue
#'
#' @examples
#' # Demo data: Calculate revenue each year from fishing licenses between 2010
#' # and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   sumRevenue(c("itemYear", "itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Calculate revenue each year from fishing licenses between 2010
#' # and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   sumRevenue(c("itemYear", "itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
sumRevenue <- function(df, groupVars = NULL) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for required columns
  reqVars <- c("price")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  df %>%
    select(!!!(syms(c("price", groupVars)))) %>%
    group_by(!!!(syms(groupVars))) %>%
    summarize(revenue = sum(price, na.rm = TRUE)) %>%
    ungroup() %>%
    collect(n = Inf) %>%
    arrange(!!!syms(groupVars)) %>%
    prettyData()
}

#' Count items by group
#'
#' Count number of items (permits, licenses, stamps, etc.) by group. This
#' function will collect data from the database if using SQL backend.
#'
#' @param df A data frame
#' @param groupVars A character vector of variable names to group by
#'
#' @return A data frame with columns for grouping variables and a column named
#' \code{items} for number of items. Data frame is passed through
#' \code{\link{prettyData}} function.
#'
#' @examples
#' # Demo data: Count number of deer licenses each year between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Deer", itemYear = c(2010, 2017))
#' ) %>%
#'   countItems(c("itemYear", "itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Count number of deer licenses each year between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Deer", itemYear = c(2010, 2017))
#' ) %>%
#'   countItems(c("itemYear", "itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
countItems <- function(df, groupVars = NULL) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  df %>%
    group_by(!!!(syms(groupVars))) %>%
    summarize(items = n()) %>%
    ungroup() %>%
    collect(n = Inf) %>%
    arrange(!!!syms(groupVars)) %>%
    prettyData()
}

#' Calculate churn
#'
#' Calculate churn using variable window size
#'
#' @param df A data frame
#' @param groupVars A character vector of variable names to group by
#' @param deathThreshold A numeric value indicating how many years
#' must elapse before an individual is considered churned
#'
#' @return A dataframe with columns for grouping variables and columns
#' churnedCustomers, retainedCustomers, totalCustomers, churnRate, and
#' retentionRate
#'
#' @examples
#' # Demo data: Churn for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcChurn(c("itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Churn for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcChurn(c("itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import tidyr
#'
#' @export
calcChurn <- function(df, groupVars = NULL, deathThreshold = 5) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for required columns
  reqVars <- c("customerUID", "itemYear")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check invalid grouping variables
  if ("itemYear" %in% groupVars) {
    stop("Item year is not a valid grouping variable")
  }

  # Check range of data
  years <- df %>%
    distinct(itemYear) %>%
    pull(itemYear)
  if ((range(years)[2] - range(years)[1] + 1) <=
    deathThreshold) {
    stop(paste0(
      "Window size is ", deathThreshold, ", but range of data",
      " is only ", range(years)[2] - range(years)[1] + 1, " years"
    ))
  }

  # SPECIAL CASE: age ang ageGroup are not used in calculating customer status
  groupVarsBefore <- groupVars[!groupVars %in% c("age", "ageGroup")]
  groupVarsAfter <- groupVars[groupVars %in% c("age", "ageGroup")]

  # Calculate years until next purchase
  nextPurchase <-
    df %>%
    distinct(!!!(syms(c("customerUID", "itemYear", groupVarsBefore)))) %>%
    group_by(!!!(syms(c("customerUID", groupVarsBefore)))) %>%
    mutate(
      nextPurchase = lead(itemYear, order_by = itemYear),
      gap = nextPurchase - itemYear - 1
    ) %>%
    ungroup()

  # Define customer status
  customerStatus <-
    nextPurchase %>%
    mutate(churnStatus = if_else(is.na(gap) | gap >= deathThreshold,
      "churnedCustomers",
      "retainedCustomers"
    ))

  # Customer covariates (join age and ageGroup if needed)
  customerCovar <- df %>%
    distinct(!!!syms(c("customerUID", "itemYear", groupVarsAfter)))

  # Calculate churn
  churn <-
    customerStatus %>%
    left_join(customerCovar,
      by = c("customerUID", "itemYear")
    ) %>%
    group_by(!!!(syms(c("itemYear", groupVars, "churnStatus")))) %>%
    summarize(N = n_distinct(customerUID)) %>%
    collect(n = Inf) %>%
    ungroup() %>%
    spread(churnStatus, N, fill = 0) %>%
    mutate(
      totalCustomers = churnedCustomers + retainedCustomers,
      churnRate = churnedCustomers / totalCustomers,
      retentionRate = 1 - churnRate
    ) %>%
    filter(itemYear <= (max(itemYear) - deathThreshold)) %>%
    prettyData()

  return(churn)
}

#' Calculate recruitment
#'
#' Calculate recruitment using a variable window size
#'
#' @param df A data frame
#' @param groupVars A character vector of variable names to group by
#' @param birthThreshold A numeric value indicating how many years
#' without purchase before an individual is considered a recruit
#'
#' @return A dataframe with columns for grouping variables and columns
#' for totalCustomers, retainedCustomers, recruitedCustomers, and recruitementRate
#'
#' @examples
#' # Demo data: Recruitment for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcRecruitment(c("itemType"))
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Recruitment for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017))
#' ) %>%
#'   calcRecruitment(c("itemType"))
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
calcRecruitment <- function(df, groupVars = NULL, birthThreshold = 5) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check groupVars class
  if (!(is.null(groupVars) | class(groupVars) == "character")) {
    stop("Input 'groupVars' should be a character vector")
  }

  # Check for required columns
  reqVars <- c("customerUID", "itemYear")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check for grouping variable columns
  if (any(!groupVars %in% colnames(df))) {
    stop(paste(
      "Grouping variables not present in data frame:",
      paste(groupVars[!groupVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Check invalid grouping variables
  if ("itemYear" %in% groupVars) {
    stop("Permit year is not a valid grouping variable")
  }

  # Check range of data
  years <- df %>%
    distinct(itemYear) %>%
    pull(itemYear)
  if ((range(years)[2] - range(years)[1] + 1) <=
    birthThreshold) {
    stop(paste0(
      "Window size is ", birthThreshold, ", but range of data",
      " is only ", range(years)[2] - range(years)[1] + 1, " years"
    ))
  }

  # SPECIAL CASE: age ang ageGroup are not used in calculating customer status
  groupVarsBefore <- groupVars[!groupVars %in% c("age", "ageGroup")]
  groupVarsAfter <- groupVars[groupVars %in% c("age", "ageGroup")]

  # Calculate years since last purchase
  lastPurchase <-
    df %>%
    distinct(!!!(syms(c("customerUID", "itemYear", groupVarsBefore)))) %>%
    group_by(!!!(syms(c("customerUID", groupVarsBefore)))) %>%
    mutate(
      lastPurchase = lag(itemYear, order_by = itemYear),
      gap = itemYear - lastPurchase - 1
    ) %>%
    ungroup()

  # Define customer status
  customerStatus <-
    lastPurchase %>%
    mutate(recruitStatus = if_else(is.na(gap) | gap >= birthThreshold,
      "recruitedCustomers",
      "retainedCustomers"
    ))

  # Customer covariates (join age and ageGroup if needed)
  customerCovar <- df %>%
    distinct(!!!syms(c("customerUID", "itemYear", groupVarsAfter)))

  # Calculate recruitment
  recruitment <-
    customerStatus %>%
    left_join(customerCovar,
      by = c("customerUID", "itemYear")
    ) %>%
    group_by(!!!(syms(c("itemYear", groupVars, "recruitStatus")))) %>%
    summarize(N = n_distinct(customerUID)) %>%
    collect(n = Inf) %>%
    ungroup() %>%
    spread(recruitStatus, N, fill = 0) %>%
    arrange(!!!(syms(c(groupVars, "itemYear")))) %>%
    group_by(!!!(syms(groupVars))) %>%
    mutate(
      totalCustomers = recruitedCustomers + retainedCustomers,
      recruitmentRate = recruitedCustomers / lag(totalCustomers)
    ) %>%
    ungroup() %>%
    filter(itemYear >= (min(itemYear) + birthThreshold)) %>%
    prettyData()

  return(recruitment)
}

#' Calculate participation rate
#'
#' Calculate participation rate by group. Participation rate is the number of
#' customers divided by the population size.
#'
#' @param df A data frame
#' @param pop A dataframe of population by county, year, gender, and age group.
#' @param groupVars A character vector of variable names to group by
#'
#' @examples
#' # Get list of valid counties (used to filter out data entry errors)
#' validCounties <- countyMaps %>%
#'   dplyr::filter(region == "nebraska") %>%
#'   dplyr::distinct(county) %>%
#'   dplyr::pull(county)
#'
#' # Demo data: Participation rate for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017), state = "NE")
#' ) %>%
#'   calcParticipation(c("itemType", "county"),
#'     pop = statePop %>% filter(state == "NE")
#'   ) %>%
#'   dplyr::filter(county %in% validCounties)
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # Get list of valid counties (used to filter out data entry errors)
#' validCounties <- countyMaps %>%
#'   dplyr::filter(region == "nebraska") %>%
#'   dplyr::distinct(county) %>%
#'   dplyr::pull(county)
#'
#' # SQL Backend: Participation rate for customers purchasing a fishing
#' # license between 2010 and 2017
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(itemType = "Fish", itemYear = c(2010, 2017), state = "NE")
#' ) %>%
#'   calcParticipation(c("itemType", "county"),
#'     pop = statePop %>% filter(state == "NE")
#'   ) %>%
#'   dplyr::filter(county %in% validCounties)
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
calcParticipation <- function(df, groupVars, pop) {

  # Count participants
  totalCount <- countCustomers(df, groupVars)

  # Define population grouping variables
  popGroupVars <- NULL
  if ("gender" %in% groupVars) {
    popGroupVars <- c(popGroupVars, "gender")
  }
  if ("county" %in% groupVars) {
    popGroupVars <- c(popGroupVars, "county")
  }
  if ("state" %in% groupVars) {
    popGroupVars <- c(popGroupVars, "state")
  }
  if ("ageGroup" %in% groupVars) {
    popGroupVars <- c(popGroupVars, "ageGroup")
  }

  # Summarize population data
  popCount <- pop %>%
    mutate(gender = as.factor(gender)) %>%
    group_by(!!!(syms(popGroupVars))) %>%
    summarize(pop = sum(population)) %>%
    ungroup()
  
  # Calculate participation rate
  participation <-
    totalCount %>%
    mutate(key = 1) %>%
    conditional_mutate(
      exists("county", where = .),
      LHS = county,
      RHS = as.character(county)
    ) %>% 
    conditional_mutate(
      exists("state", where = .),
      LHS = state,
      RHS = as.character(state)
    ) %>% 
    left_join(
      popCount %>%
        mutate(key = 1) %>%
        conditional_mutate(
          exists("county", where = .),
          LHS = county,
          RHS = as.character(county)
        )%>%
        conditional_mutate(
          exists("state", where = .),
          LHS = state,
          RHS = as.character(state)
        ),
      by = c("key", popGroupVars)
    ) %>% 
    select(-key) %>%
    group_by(!!!(syms(groupVars))) %>%
    mutate(participationRate = customers / pop) %>%
    ungroup() %>%
    prettyData()

  # Return summarized data
  return(participation)
}

#' Count customers by permit groups
#'
#' Count customers who bought each combination of permits and stamps
#'
#' @param df A data frame
#'
#' @examples
#' # Demo data: Count number of customers by combination of items purchased
#' filterData(
#'   dataSource = "csv",
#'   activeFilters = list(
#'     itemType = c("Fish", "Deer", "Spring Turkey"),
#'     itemYear = c(2018, 2018)
#'   )
#' ) %>%
#'   itemGroupCount()
#' \dontrun{
#' # Database connection. Suggest using keyring package to avoid hardcoding
#' # passwords
#' myConn <- DBI::dbConnect(odbc::odbc(),
#'   dsn = "HuntFishApp", # Your datasource name
#'   uid = keyring::key_get("HuntFishAppUID"), # Your username
#'   pwd = keyring::key_get("HuntFishAppPWD")
#' ) # Your password
#'
#' # SQL Backend: Count number of customers by combination of items purchased
#' filterData(
#'   dataSource = "sql",
#'   conn = myConn,
#'   activeFilters = list(
#'     itemType = c("Fish", "Deer", "Spring Turkey"),
#'     itemYear = c(2018, 2018)
#'   )
#' ) %>%
#'   itemGroupCount()
#' }
#'
#' @family analysis functions
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
itemGroupCount <- function(df) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop(paste("Input 'df' should be a data frame"))
  }

  # Check for required columns
  reqVars <- c("customerUID", "itemType")
  if (any(!reqVars %in% colnames(df))) {
    stop(paste(
      "Required variables not present in data frame:",
      paste(reqVars[!reqVars %in% colnames(df)], collapse = ", ")
    ))
  }

  # Get list of permit types
  permitTypeLabels <- df %>%
    distinct(itemType) %>%
    pull(itemType)

  # Drop duplicate rows and group data
  wideDf <- df %>%
    distinct(!!!(syms(c("customerUID", "itemType")))) %>%
    group_by(!!!(syms(c("customerUID"))))

  # Create indicator variables
  for (i in 1:length(permitTypeLabels)) {
    wideDf <- wideDf %>%
      mutate(!!sym(permitTypeLabels[i]) :=
        max(if_else(itemType == !!permitTypeLabels[i], 1, 0), na.rm = T))
  }

  # Summarize data
  wideData <-
    wideDf %>%
    ungroup() %>%
    select(-itemType) %>%
    distinct() %>%
    countCustomers(permitTypeLabels) %>%
    arrange(desc(customers)) %>%
    mutate(groupID = row_number()) %>%
    select(groupID, everything())

  # Convert data to long form
  longData <- wideData %>%
    gather(itemType, purchase, !!!syms(permitTypeLabels)) %>%
    arrange(groupID) %>%
    group_by(groupID) %>%
    mutate(degree = sum(purchase)) %>%
    ungroup()

  return(longData)
}

#' Count shared customers
#'
#' @export
countSharedCustomers <- function(comboCount) {

  # Spread count to wide form
  comboCountWide <-
    comboCount %>%
    spread(itemType, purchase)

  # Create matrix of shared customers between groups
  itemTypes <- unique(comboCount[["itemType"]])
  nTypes <- length(itemTypes)
  edges <- matrix(
    data = 0,
    nrow = nTypes,
    ncol = nTypes
  )
  rownames(edges) <- itemTypes
  colnames(edges) <- itemTypes
  for (i in c(1:nTypes)) {
    for (j in c(1:nTypes)) {
      edges[i, j] <-
        comboCountWide %>%
        filter(
          UQ(sym(itemTypes[i])) == 1,
          UQ(sym(itemTypes[j])) == 1
        ) %>%
        summarize(customers = sum(customers)) %>%
        pull(customers)
    }
  }

  # Convert matrix to tibble and calculate proportions
  sharedCustomers <- as_tibble(edges) %>%
    mutate(type1 = row.names(edges)) %>%
    gather(type2, customers, -type1) %>%
    arrange(type1) %>%
    group_by(type1) %>%
    mutate(total1 = customers[type1 == type2]) %>%
    group_by(type2) %>%
    mutate(total2 = customers[type1 == type2]) %>%
    ungroup() %>%
    mutate(
      prop1 = customers / total1,
      prop2 = customers / total2,
      prop = if_else(type1 != type2, customers / (total1 + total2), 1),
      type1 = factor(type1),
      type2 = factor(type2, levels = levels(type1))
    )
}

#' Count customers by number of other item types
#'
#' @export
countItemTypes <- function(comboCount) {

  # Spread count to wide form
  comboCountWide <-
    comboCount %>%
    spread(itemType, purchase)

  # Create matrix of customer count by number of other item types purchased
  itemTypes <- unique(comboCount[["itemType"]])
  nTypes <- length(itemTypes)
  maxDegree <- 4
  countByItemTypes <- matrix(
    data = 0,
    nrow = nTypes,
    ncol = maxDegree
  )
  rownames(countByItemTypes) <- itemTypes
  colnames(countByItemTypes) <- 1:maxDegree
  for (i in c(1:nTypes)) {
    for (j in c(1:maxDegree)) {
      countByItemTypes[i, j] <-
        comboCountWide %>%
        filter(
          UQ(sym(itemTypes[i])) == 1,
          degree == j |
            ((j == maxDegree) & degree >= j)
        ) %>%
        summarize(customers = sum(customers)) %>%
        pull(customers)
    }
  }

  # Convert matrix to tibble and calculate proportions
  degreeCount <-
    as_tibble(countByItemTypes) %>%
    mutate(itemType = row.names(countByItemTypes)) %>%
    gather(degree, customers, -itemType) %>%
    arrange(itemType) %>%
    group_by(itemType) %>%
    mutate(total = sum(customers)) %>%
    ungroup() %>%
    mutate(prop = customers / total) %>%
    mutate(
      degreeLabel = if_else(degree == "4", "4+", degree),
      degreeLabel = factor(degreeLabel),
      degree = as.numeric(degree),
      itemType = factor(itemType)
    )
}

#' Calculate edge matrix for Radial Sets plot
#'
#' @export
radialSetsData <- function(sharedCustomers,
                           degreeCount,
                           linkThickness = "percent",
                           focusGroup = "none",
                           countScale = 1 / 1e3) {

  # Types of items
  itemTypes <- levels(degreeCount$itemType)
  nTypes <- length(itemTypes)

  # Maximum degree
  maxDegree <- max(degreeCount[["degree"]])

  # Convert degree count to matrix
  degreeCountMat <-
    degreeCount %>%
    arrange(itemType) %>%
    select(itemType, degree, customers) %>%
    mutate(customers = customers * countScale) %>%
    spread(degree, customers) %>%
    select(-itemType) %>%
    as.matrix()
  rownames(degreeCountMat) <- itemTypes
  colnames(degreeCountMat) <- 1:maxDegree

  # Number of customers with each type
  totalVec <- degreeCount %>%
    arrange(itemType) %>%
    distinct(itemType, total) %>%
    mutate(total = total * countScale) %>%
    pull(total)
  names(totalVec) <- itemTypes

  # Edges matrix
  if (linkThickness != "percent") {

    # Calculate edges
    edges <-
      sharedCustomers %>%
      arrange(type1) %>%
      select(type1, type2, customers) %>%
      mutate(customers = customers) %>%
      spread(type2, customers) %>%
      select(-type1) %>%
      as.matrix()
    rownames(edges) <- itemTypes

    if (focusGroup != "none") {
      # Remove all links besides focus group
      edges[-which(itemTypes == focusGroup), ] <- 0
    }

    # Scale edges by thousands
    edges <- edges * countScale
  } else if (linkThickness == "percent") {
    if (focusGroup == "none") {
      edges <-
        sharedCustomers %>%
        arrange(type1) %>%
        select(type1, type2, prop) %>%
        spread(type2, prop) %>%
        select(-type1) %>%
        as.matrix()
      rownames(edges) <- itemTypes
    } else {
      edges <-
        sharedCustomers %>%
        arrange(type1) %>%
        select(type1, type2, prop1) %>%
        spread(type2, prop1) %>%
        select(-type1) %>%
        as.matrix()
      rownames(edges) <- itemTypes

      # Remove all links besides focus group
      edges[-which(itemTypes == focusGroup), ] <- 0
    }

    # Scale edges to percent
    edges <- edges * 100
  }

  # Remove self-links
  diag(edges) <- 0

  # Maximum edge width
  maxWidth <- signif(max(edges), 1)

  return(list(
    edges = edges,
    itemTypes = itemTypes,
    nTypes = nTypes,
    maxDegree = maxDegree,
    degreeCountMat = degreeCountMat,
    totalVec = totalVec,
    maxWidth = maxWidth
  ))
}
