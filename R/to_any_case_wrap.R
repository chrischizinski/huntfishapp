#' Wrap to_any_case function
#'
#' Wrapper for \code{\link[snakecase]{to_any_case}} function that returns NULL
#' if input string is NULL
#'
#' @param df A dataframe
#'
#' @importFrom snakecase to_any_case
#'
to_any_case_wrap <- function(string, case) {
  if (is.null(string)) {
    return(NULL)
  } else {
    return(to_any_case(string, case))
  }
}
