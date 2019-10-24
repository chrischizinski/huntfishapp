#' Calculate plot height
#'
#' Calculate plot height for different page widths and number of subplots
#'
#' @param plotData A dataframe of data used in plot
#' @param pageWidth Width of page in pixels
#' @param facetVars A character vector with names of facet variables
#'
#' @export
calcualtePlotHeight <- function(plotData, pageWidth, facetVars) {

  # Plot width
  width <- min(1366, pageWidth) - 60

  # Logic to handle facets
  if (length(facetVars) == 0) {
    height <- 380
  } else {

    # Number of subplots
    nLevel <- length(unique(plotData[[facetVars[1]]]))

    # Number of rows
    nCols <- 2
    nRows <- max(1, ceiling(nLevel / nCols))

    # Define aspect ratio
    aspectRatio <- 0.575

    # Calculate plot height
    if (nRows == 1) {
      height <- 380
    } else if (nRows >= 2) {
      height <- nRows * aspectRatio * width / nCols
    }
  }

  # Return height
  return(height)
}
