
#' Tooltip for bar plots
#'
#' Build tooltip for bar plots
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#' @importFrom forcats fct_explicit_na
#'
#' @export
createBarTooltip <- function(plotData, hover) {

  # Extract mappings
  x <- hover$mapping$x
  y <- hover$mapping$y
  facet <- hover$mapping$panelvar1
  fill <- hover$mapping$fill

  # If x-variable is NULL, return NULL
  if (is.null(hover$x) | is.null(x)) {
    return(NULL)
  } else if (is.null(plotData[[x]])) {
    return(NULL)
  }

  # Check that x is factor
  if (!(is.Date(plotData[[x]]) | is.factor(plotData[[x]]))) {
    stop("The x-variable should be a factor")
  }

  # Get labels
  xLabel <- to_any_case_wrap(x, "sentence")
  yLabel <- to_any_case_wrap(sub("Scaled", "", y), "sentence")
  facetLabel <- to_any_case_wrap(facet, "sentence")
  fillLabel <- to_any_case_wrap(fill, "sentence")

  # Grouping variables
  groupVars <- c(x, facet)
  groupVars <- unique(groupVars[groupVars != "none"])

  # Prepare plot data
  plotData <-
    plotData %>%
    mutate_if(is.factor, forcats::fct_explicit_na) %>%
    scaleVariable(sub("Scaled", "", y), groupVars)

  # Get x-variable value
  if (is.factor(plotData[[x]])) {
    xVal <- levels(plotData[[x]])[round(hover$x)]
  } else if (is.Date(plotData[[x]])) {
    xVal <- as.Date(round(hover$x), origin = "1970-01-01")
  }

  # Calculate cumulative height of bar
  if (!is.null(fill)) {
    if (!is.null(facet)) {

      # Fill variable / Subplots
      plotData <-
        plotData %>%
        arrange(!!!(syms(c(
          x, facet
        ))), desc(!!(sym(fill)))) %>%
        group_by(!!!(syms(c(
          x, facet
        )))) %>%
        mutate(cValue = cumsum(!!sym(y)))
    } else {

      # Fill variable / No subplots
      plotData <-
        plotData %>%
        arrange(!!(sym(x)), desc(!!(sym(fill)))) %>%
        group_by(!!!(syms(c(x)))) %>%
        mutate(cValue = cumsum(!!sym(y)))
    }
  } else {
    if (!is.null(facet)) {

      # No fill variable / Subplots
      plotData <-
        plotData %>%
        arrange(!!(sym(x)), desc(!!(sym(facet)))) %>%
        group_by(!!!(syms(c(x)))) %>%
        mutate(cValue = cumsum(!!sym(y)))
    } else {

      # No fill variable / No subplots
      plotData <-
        plotData %>%
        mutate(cValue = !!sym(y))
    }
  }

  # Get point corresponding to hover location
  panelVar <- if_else(is.null(hover$panelvar1), "NA", hover$panelvar1)
  if (!is.null(facet)) {
    # Filter based on x-variable, y-variable, and facet
    point <- plotData %>%
      filter(
        !!(sym(x)) == xVal,
        !!(sym(facet)) == panelVar,
        cValue >= hover$y
      ) %>%
      filter(cValue == min(cValue))
  } else {
    # Filter based on x-variable and y-variable
    point <- plotData %>%
      filter(
        !!(sym(x)) == xVal,
        cValue >= hover$y
      ) %>%
      filter(cValue == min(cValue))
  }

  # If no matching points, return NULL
  if (nrow(point) == 0) {
    return(NULL)
  }

  # Extract values
  yVal <- point[[y]]
  if (!is.null(facet)) {
    facetVal <- point[[facet]]
  }
  if (!is.null(fill)) {
    fillVal <- point[[fill]]
  }

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <-
    (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <-
    (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <-
    hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <-
    hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # Set background color to match fill variable
  if (!is.null(fill)) {
    pal <- barAndLinePal(fill)
    fillColors <-
      brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    bgColorHex <- if (fillVal == "NA") "grey50" else fillColors[as.numeric(fillVal)]
    bgColor <-
      paste0("rgba(", paste(col2rgb(bgColorHex), collapse = ","), ", 0.85)")
  } else {
    bgColor <- "rgba(245, 245, 245, 0.85)"
  }

  # Create style property fot tooltip (transparent and on top)
  style <-
    paste0(
      "position:absolute;",
      "z-index:100; border-color: black; background-color: ",
      bgColor,
      "; left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )

  # Tooltip created as wellPanel
  tooltipPanel <- wellPanel(
    style = style,
    p(HTML(
      paste0(
        "<b>", xLabel, "</b>: ", xVal,
        if (!is.null(facet)) {
          paste0("<br/><b>", facetLabel, "</b>: ", facetVal)
        },
        if (!is.null(fill)) {
          paste0("<br/><b>", fillLabel, "</b>: ", fillVal)
        },
        "<br/><b>",
        yLabel, "</b>: ", signif(yVal, 3)
      )
    ))
  )

  return(tooltipPanel)
}

#' Tooltip for line plots
#'
#' Buid tooltip for line plots
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#' @importFrom forcats fct_explicit_na
#'
#' @export
createLineTooltip <- function(plotData, hover) {

  # Extract mappings
  x <- hover$mapping$x
  y <- hover$mapping$y
  facet <- hover$mapping$panelvar1
  fill <- hover$mapping$colour

  # If x-variable is NULL, return NULL
  if (is.null(hover$x) | is.null(plotData[[x]])) {
    return(NULL)
  }

  # Get labels
  xLabel <- to_any_case_wrap(x, "sentence")
  yLabel <- to_any_case_wrap(sub("Scaled", "", y), "sentence")
  facetLabel <- to_any_case_wrap(facet, "sentence")
  fillLabel <- to_any_case_wrap(fill, "sentence")

  # Grouping variables
  groupVars <- c(x, facet, fill)
  groupVars <- unique(groupVars[groupVars != "none"])

  # Prepare plot data
  plotData <-
    plotData %>%
    mutate_if(is.factor, forcats::fct_explicit_na) %>%
    scaleVariable(sub("Scaled", "", y), groupVars)

  # Get nearby point
  point <- nearPoints(plotData, hover, threshold = 5, maxpoints = 1)

  # If not matching points, return NULL
  if (nrow(point) == 0) {
    return(NULL)
  }

  # Extract values
  xVal <- point[[x]]
  yVal <- point[[y]]
  if (!is.null(facet)) {
    facetVal <- point[[facet]]
  }
  if (!is.null(fill)) {
    fillVal <- point[[fill]]
  }

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <-
    (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <-
    (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <-
    hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <-
    hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  if (!is.null(fill)) {
    pal <- barAndLinePal(fill)
    fillColors <- brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    bgColorHex <- if (fillVal == "NA") "grey50" else fillColors[as.numeric(fillVal)]
    bgColor <- paste0("rgba(", paste(col2rgb(bgColorHex), collapse = ","), ", 0.85)")
  } else {
    bgColor <- "rgba(245, 245, 245, 0.85)"
  }

  style <-
    paste0(
      "position:absolute;",
      "z-index:100; border-color: black; background-color: ",
      bgColor,
      "; left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )

  # actual tooltip created as wellPanel
  tooltipPanel <- wellPanel(
    style = style,
    p(HTML(
      paste0(
        "<b>", xLabel, "</b>: ", xVal,
        if (!is.null(facet)) paste0("<br/><b>", facetLabel, "</b>: ", facetVal),
        if (!is.null(fill)) paste0("<br/><b>", fillLabel, "</b>: ", fillVal),
        "<br/><b>", yLabel, "</b>: ", signif(yVal, 3)
      )
    ))
  )

  return(tooltipPanel)
}

#' Tooltip for map plots
#'
#' Build tooltip for map plots
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#'
#' @export
createMapTooltip <- function(plotData, hover, yvar) {

  # If x-variable is NULL, return NULL
  if (is.null(hover$x)) {
    return(NULL)
  }

  point <- nearPoints(
    df = plotData,
    coordinfo = hover,
    xvar = "longCent",
    yvar = "latCent",
    panelvar1 = hover$mapping$panelvar1,
    threshold = 100,
    maxpoints = 1,
    addDist = T
  )

  fill <- hover$mapping$fill
  fillLabel <- to_any_case_wrap(fill, "sentence")

  if (grepl("Rate", fill)) {
    point <- point %>%
      mutate(!!sym(fill) := !!sym(fill) * 100)
  }

  # If not matching points, return NULL
  if (nrow(point) == 0) {
    return(NULL)
  }

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <-
    (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <-
    (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <-
    hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <-
    hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <-
    paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )

  # actual tooltip created as wellPanel
  tooltipPanel <- wellPanel(
    style = style,
    p(HTML(
      paste0(
        "<b>",
        "Region",
        "</b>: ",
        point[["label"]],
        "<br/>",
        "<b>",
        fillLabel,
        "</b>: ",
        signif(point[[fill]], 3)
      )
    ))
  )

  return(tooltipPanel)
}

#' Tooltip for radial sets plot
#'
#' Build tooltip for radial sets network plot
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#'
#' @export
createRadialsetsTooltip <- function(sharedCustomers,
                                    degreeCount,
                                    hover,
                                    focusGroup = "none",
                                    linkThickness = "percent",
                                    countScale = 1 / 1e3) {

  # Unpack coordinates of mouse pointer
  x <- hover$x
  y <- hover$y

  # If mouse is not on plot, return null
  if (!is.numeric(x)) {
    return(NULL)
  }

  # Convert pointer location to polar coordinates
  r <- sqrt(x^2 + y^2)
  theta <- atan2(y, x)

  # Convert angle in radians to degrees
  if (theta > 0) {
    deg <- theta * (180 / pi)
  } else {
    deg <- 360 + theta * (180 / pi)
  }

  # Get plot data
  networkData <- radialSetsData(sharedCustomers,
    degreeCount,
    linkThickness = linkThickness,
    focusGroup = focusGroup,
    countScale = countScale
  )

  # Unpack data
  edges <- networkData$edges
  itemTypes <- networkData$itemTypes
  nTypes <- networkData$nTypes
  maxDegree <- networkData$maxDegree
  degreeCountMat <- networkData$degreeCountMat
  totalVec <- networkData$totalVec
  maxWidth <- networkData$maxWidth

  # Loop over sectors collecting sector data
  secNames <- get.all.sector.index()
  xplot <- matrix(nrow = length(secNames), ncol = 2)
  yplot <- matrix(nrow = length(secNames), ncol = 2)
  xlim <- matrix(nrow = length(secNames), ncol = 2)
  xToDeg <- vector(mode = "numeric", length = length(secNames))
  for (i in c(1:length(secNames))) {
    xplot[i, ] <- get.cell.meta.data("xplot", sector.index = secNames[i], track.index = 2)
    yplot[i, ] <- get.cell.meta.data("yplot", sector.index = secNames[i], track.index = 2)
    xlim[i, ] <- get.cell.meta.data("xlim", sector.index = secNames[i], track.index = 2)
  }
  xplot[1, 1] <- 360
  xToDeg <- (xplot[, 1] - xplot[, 2]) / (xlim[, 2] - xlim[, 1])

  # Loop over links collecting points for each link
  links <- list()
  k <- 1
  linkInd <- matrix(nrow = nTypes, ncol = nTypes)
  for (i in c(1:nTypes)) {
    for (j in c(1:nTypes)) {
      if (edges[i, j] != 0) {
        sector.index1 <- itemTypes[i]
        sector.index2 <- itemTypes[j]
        point1 <- totalVec[i] / 2
        point2 <- totalVec[j] / 2
        rou <- min(get.cell.meta.data("yplot", sector.index = sector.index1, track.index = 2))

        theta1 <- circlize(point1, 0,
          sector.index = sector.index1,
          track.index = 0
        )[1, "theta"]
        theta2 <- circlize(point2, 0,
          sector.index = sector.index2,
          track.index = 0
        )[1, "theta"]
        links[[k]] <- circlize:::getQuadraticPoints(theta1, theta2, rou1 = rou, rou2 = rou, h = NULL, w = 1)
        linkInd[i, j] <- k
        k <- k + 1
      }
    }
  }

  # If pointer is in center of plot, return tooltip for links
  if ((r < min(yplot)) | (r > max(yplot))) {
    for (i in c(1:length(links))) {

      # Dataframe of points for ith link
      df <- data_frame(xvar = links[[i]][, 1], yvar = links[[i]][, 2])

      # Get matrix of nearby points
      pointsList <- nearPoints(df, hover, xvar = "xvar", yvar = "yvar", addDist = TRUE)

      # If found nearby link, stop search
      if (!nrow(pointsList) == 0) {
        break
      }
    }

    # If search ended with no nearby links return null
    if (nrow(pointsList) == 0) {
      return(NULL)
    }

    # Match the link index to the matrix of overlaps
    ind <- which(linkInd == i, arr.ind = T)

    # Create string displaying overlap for given link
    name1 <- secNames[ind[1]]
    name2 <- secNames[ind[2]]
    linkName <- paste0(
      name1,
      ifelse(focusGroup == "none", " and ", " to "),
      name2
    )

    overlap <- edges[ind[1], ind[2]]
    if (linkThickness != "percent") {
      overlap <- overlap * (1 / countScale)
    }
    overlap <- round(overlap)
    if (focusGroup == "none") {
      if (linkThickness == "percent") {
        label <- paste0(
          overlap, "% of all ", name1, " and ", name2,
          "</br>customers purchased both"
        )
      } else {
        label <- paste0(
          overlap, " customers purchased both</br>",
          name1, " and ", name2
        )
      }
    } else {
      if (linkThickness == "percent") {
        label <- paste0(
          overlap, "% of ", name1,
          " customers</br>also purchased ", name2
        )
      } else {
        label <- paste0(
          overlap, " ", name1, " customers</br>also purchased ",
          name2
        )
      }
    }

    tooltipText <- paste0("<b>", linkName, "</b> <br/>", label)
  } else {


    # Determine which sector the pointer is in
    secInd <- which((deg < xplot[, 1]) & (deg > xplot[, 2]) & (r > yplot[, 1]) & (r < yplot[, 2]))
    if (length(secInd) == 0) {
      return(NULL)
    }

    # Calculate bounds for each radial histogram bar
    rlim <- vector(mode = "numeric", length = maxDegree + 1)
    for (i in c(1:(maxDegree + 1))) {
      rlim[i] <- min(yplot) + ((i - 1) / maxDegree) * (max(yplot) - min(yplot))
    }

    # Determine which histogram bar the pointer is in
    r_lb <- rlim[maxDegree:1]
    r_ub <- rlim[(maxDegree + 1):2]
    histInd <- which(r >= r_lb & r < r_ub)

    # Bar value
    customerValue <- degreeCount %>%
      filter(
        itemType == secNames[secInd],
        degree == histInd
      ) %>%
      pull(customers)

    # Bar proportion
    propValue <- degreeCount %>%
      filter(
        itemType == secNames[secInd],
        degree == histInd
      ) %>%
      mutate(prop = round(prop * 100)) %>%
      pull(prop)

    # Sector value
    totalValue <- degreeCount %>%
      filter(
        itemType == secNames[secInd],
        degree == histInd
      ) %>%
      mutate(prop = round(prop * 100)) %>%
      pull(total)

    # If pointer is on histogram bar, display bar value, else display sector total
    s <- degreeCountMat[secInd, histInd] * xToDeg[secInd]
    xc <- xplot[secInd, 2] + (xplot[secInd, 1] - xplot[secInd, 2]) / 2
    if ((deg <= xc + s / 2) & (deg >= xc - s / 2)) {
      if (histInd == maxDegree) {
        tooltipText <- paste0(
          "<b>", secNames[secInd], "</b> <br/>",
          secNames[secInd], " and ", histInd - 1, " or more other types: ", customerValue,
          " (", propValue, "%)"
        )
      } else if (histInd == 1) {
        tooltipText <- paste0(
          "<b>", secNames[secInd], "</b> <br/>",
          secNames[secInd], " only: ", customerValue,
          " (", propValue, "%)"
        )
      } else {
        tooltipText <- paste0(
          "<b>", secNames[secInd], "</b> <br/>",
          secNames[secInd], " and ", histInd - 1, " other type(s): ", customerValue,
          " (", propValue, "%)"
        )
      }
    } else {
      tooltipText <- paste0("<b>", secNames[secInd], "</b> <br/>", totalValue, " customers")
    }
  }

  # Calculate point position INSIDE the image as percent of total dimensions
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # Calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # Create style property fot tooltip (transparent background, tooltip on top)
  style <- paste0(
    "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;"
  )

  # Create tooltip as wellPanel
  tooltipPanel <- wellPanel(
    style = style,
    p(HTML(tooltipText))
  )

  return(tooltipPanel)
}
