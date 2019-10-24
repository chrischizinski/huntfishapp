#' Range filter function
#'
#' Apply a filter on range of variable if condition is met This function is a simple wrapper for
#' \code{\link[dplyr]{filter}}
#'
#' @param df1 A dataframe
#' @param cond A logical value indicating if filter should be applied
#' @param varName Name of variable to filter on
#' @param varCond A vector with lower and upper bound on variable. Use NA
#' values to perform a one sided filter.
#'
#' @examples
#' data(starwars, package = "dplyr")
#' starwars %>%
#'   conditional_range_filter(TRUE, "height", c(160, 180))
#'
#' starwars %>%
#'   conditional_range_filter(TRUE, "height", c(180, NA))
#' @family conditional dplyr
#'
#' @import dplyr
#' @import rlang
#'
#' @export
conditional_range_filter <- function(df1, cond, varName, varCond) {
  if (cond) {
    if (length(varCond) != 2) {
      stop("Range filter requires two filter values (minimum and maximum). Use NA for no filters.")
    }

    minVal <- varCond[1]
    maxVal <- varCond[2]
    if (!is.na(minVal) & !is.na(maxVal)) {
      if (minVal > maxVal) {
        stop("First filter value should be less than or equal to second filter value.")
      }
      filter(df1, UQ(sym(varName)) >= minVal) %>%
        filter(UQ(sym(varName)) <= maxVal)
    } else if (!is.na(minVal)) {
      filter(df1, UQ(sym(varName)) >= minVal)
    } else if (!is.na(maxVal)) {
      filter(df1, UQ(sym(varName)) <= maxVal)
    }
  } else {
    df1
  }
}

#' Conditional filter function
#'
#' Apply a filter if condition is met. This function is a simple wrapper for
#' \code{\link[dplyr]{filter}}
#'
#' @param df1 A dataframe
#' @param cond A logical value indicating if filter should be applied
#' @param varName Name of variable to filter on
#' @param varCond A single value for equality filter or a vector for
#' is member filter
#'
#' @examples
#' data(starwars, package = "dplyr")
#' starwars %>%
#'   conditional_filter(TRUE, "species", "Human")
#'
#' starwars %>%
#'   conditional_filter(TRUE, "species", c("Human", "Droid"))
#' @family conditional dplyr
#'
#' @import dplyr
#' @import rlang
#'
#' @export
conditional_filter <- function(df1, cond, varName, varCond) {
  if (cond) {
    if (length(varCond) == 1) {
      filter(df1, UQ(sym(varName)) == varCond)
    } else {
      filter(df1, UQ(sym(varName)) %in% varCond)
    }
  } else {
    df1
  }
}

#' Conditional mutate function
#'
#' Perform mutate if condition is met. This function is a simple wrapper for
#' \code{\link[dplyr]{mutate}}.
#'
#' @param df1 A dataframe
#' @param cond A logical value indicating if mutate should be performed
#' @param varName Name of variable to create
#'
#' @examples
#' mtcars %>%
#'   dplyr::as_tibble() %>%
#'   conditional_mutate(
#'     exists("cyl", where = .),
#'     LHS = cyl2,
#'     RHS = cyl * 2
#'   )
#'
#' mtcars %>%
#'   dplyr::as_tibble() %>%
#'   conditional_mutate(
#'     exists("cyl2", where = .),
#'     LHS = cyl4,
#'     RHS = cyl2 * 2
#'   )
#' @family conditional dplyr
#'
#' @import dplyr
#'
#' @export
conditional_mutate <- function(df1, cond, LHS, RHS) {
  user_expr <- enquo(RHS)
  new_col <- quo_name(enquo(LHS))

  if (cond) {
    df1 %>%
      mutate(!!new_col := !!user_expr)
  } else {
    df1
  }
}

#' Conditional select function
#'
#' Perform select if condition is met. This function is a simple wrapper for
#' \code{\link[dplyr]{select}}
#'
#' @param df1 A dataframe
#' @param cond A logical value indicating if select should be performed
#' @param user_expr Select expression
#'
#' @examples
#' mtcars %>%
#'   dplyr::as_tibble() %>%
#'   conditional_select(
#'     exists("cyl", where = .),
#'     user_expr = -cyl
#'   )
#' @family conditional dplyr
#'
#' @import dplyr
#'
#' @export
conditional_select <- function(df1, cond, user_expr) {
  user_expr <- enquo(user_expr)

  if (cond) {
    df1 %>%
      select(!!user_expr)
  } else {
    df1
  }
}
