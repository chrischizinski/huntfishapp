
#' Palettes for bar and line plots
#'
#' Define color palette for each variable used in bar and line plots
#'
#' @param fill Name of color or fill variable (string)
#'
#' @return A character vector specifying a palette
#'
#' @export
barAndLinePal <- function(fill) {

  # Valid fill variables
  fillVars <- c(
    "itemType",
    "gender",
    "residency",
    "itemResidency",
    "duration",
    "ageGroup"
  )

  # Default palette
  if (!(fill %in% fillVars)) {
    return("Paired")
  }

  # Switch palette based on variable name
  switch(
    fill,
    "itemType" = "Paired",
    "gender" = "Dark2",
    "residency" = "Pastel1",
    "itemResidency" = "Pastel1",
    "duration" = "Set1",
    "ageGroup" = "Set2"
  )
}

#' Build a line plot
#'
#' This function builds a line plot given user specifications using ggplot2.
#'
#' The plot labels are based on variable names. The
#' \code{\link[snakecase]{to_any_case}} function is used to convert variable
#' names from the data frame in camel case to sentence case.
#'
#' The y-variable values are divided by one thousand or one million when
#' appropriate to simplify axis labels. Scaling is performed using the
#' \code{\link{scaleVariable}} function.
#'
#' The y-axis labels are repeated for each
#' subplot using the \code{\link[lemon]{facet_rep_wrap}} function.
#'
#' The \code{\link{barAndLinePal}} function is used to map the fill variable
#' to a fill color.
#'
#' If the maximum length of x-variable labels are greater than or equal to
#' four, then the labels are rotated 45 degrees.
#'
#' @param df A data frame with plot data
#' @param x Name of x-variable (string)
#' @param y Name of y-variable (string)
#' @param fill Name of fill variable (string)
#' @param facet Name of facet variable (string)
#' @param title Title of plot (string)
#' @param facetScales Scale specification for subplots (fixed, free_y, free_x, free).
#' This value is passed to the \code{\link[lemon]{facet_rep_wrap}} function.
#' @param yLimits A vector specifying y-axis limits for use with
#' \code{yScales = "manual"}. Use \code{NA} to leave a limit unspecified.
#' @param yScales A string specifying y-axis scale type. Possible values are
#' "zero" for a zero lower limit and upper limit fit to data, "manual" for a manual
#' specification given by yLimits, or NULL for limits fit to data
#' @param scaleLabels Scale labels for y-variable (e.g., waiver(), scales::percent).
#' This value is passed to the \code{\link[ggplot2]{scale_y_continuous}} function.
#'
#' @return A ggplot2 plot object
#'
#' @family plot functions
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lemon facet_rep_wrap
#' @importFrom snakecase to_any_case
#'
#' @export
buildLinePlot <-
  function(df,
             x,
             y,
             fill = "none",
             facet = "none",
             title = NULL,
             facetScales = "fixed",
             yLimits = c(NA, NA),
             yScales = "zero",
             scaleLabels = waiver()) {

    # Call function to calculate scaling
    groupVars <- c(x, facet, fill)
    groupVars <- unique(groupVars[groupVars != "none"])
    df <- df %>% scaleVariable(y, groupVars)

    # Add unit to y-labels
    scaleLabel <- case_when(
      df$divisor[1] == 1e3 ~ "\n(thousand)",
      df$divisor[1] == 1e6 ~ "\n(million)",
      TRUE ~ ""
    )
    yLabel <- paste(to_any_case(y, "sentence"), scaleLabel)
    xLabel <- to_any_case(x, "sentence")
    fillLabel <- to_any_case(fill, "sentence")

    # Initialize plot
    if (fill != "none") {
      g <- ggplot(
        data = df,
        aes_string(
          x = x,
          y = paste0(y, "Scaled"),
          group = fill,
          fill = fill,
          color = fill
        )
      )
    } else {
      g <- ggplot(
        data = df,
        aes_string(
          x = x,
          y = paste0(y, "Scaled"),
          group = 1
        )
      )
    }

    # Add selected geoms
    g <- g + geom_point(size = 2) + geom_line(size = 1.5)

    # Add facets to plot
    if (facet != "none") {
      g <- g + lemon::facet_rep_wrap(
        as.formula(paste("~", facet)),
        ncol = 2,
        scales = facetScales,
        repeat.tick.labels = "all"
      )
    }

    # Add labels and color scales
    if (fill != "none") {
      g <- g + labs(x = xLabel, y = yLabel, fill = fillLabel, color = fillLabel, title = title) +
        scale_fill_brewer(palette = barAndLinePal(fill), na.value = "grey50") +
        scale_color_brewer(palette = barAndLinePal(fill), na.value = "grey50")
    } else {
      g <- g + labs(x = xLabel, y = yLabel, title = title)
    }

    # x-axis label angle
    xLabAngle <- if_else(max(nchar(as.character(df[[x]]))) >= 4, 45, 0)
    xHJust <- if_else(xLabAngle == 45, 1, 0.5)

    # Finalize plot
    g <- g +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2])),
        expand = expand_scale(mult = 0.05)
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = xLabAngle, hjust = xHJust),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        aspect.ratio = 1 / 2
      )

    # Set ylim
    if (yScales == "zero") {
      g <- g + scale_y_continuous(labels = scaleLabels, limits = c(0, NA))
    } else if (yScales == "manual") {
      g <- g + scale_y_continuous(labels = scaleLabels, limits = yLimits / df$divisor[1])
    } else {
      g <- g + scale_y_continuous(labels = scaleLabels)
    }

    # Return plot
    return(g)
  }

#' Build a bar plot
#'
#' This function builds a bar plot given user specifications using ggplot2.
#'
#' The plot labels are based on variable names. The
#' \code{\link[snakecase]{to_any_case}} function is used to convert variable
#' names from the data frame in camel case to sentence case.
#'
#' The y-variable values are divided by one thousand or one million when
#' appropriate to simplify axis labels. Scaling is performed using the
#' \code{\link{scaleVariable}} function.
#'
#' The y-axis labels are repeated for each
#' subplot using the \code{\link[lemon]{facet_rep_wrap}} function.
#'
#' The \code{\link{barAndLinePal}} function is used to map the fill variable
#' to a fill color.
#'
#' This function looks for a variable named \code{paste0(y, "Avg")} in the
#' data frame. If this variable is available, it is used to create a
#' horizontal line indicating the average value. A variable \code{avgLab}
#' should also be present in the data frame with the legend text for the
#' horizontal line.
#'
#' If the maximum length of x-variable labels are greater than or equal to
#' four, then the labels are rotated 45 degrees.
#'
#' @param df A data frame with plot data
#' @param x Name of x-variable (string)
#' @param y Name of y-variable (string)
#' @param fill Name of fill variable (string)
#' @param facet Name of facet variable (string)
#' @param title Title of plot (string)
#' @param facetScales Scale specification for subplots (fixed, free_y, free_x, free).
#' This value is passed to the \code{\link[lemon]{facet_rep_wrap}} function.
#' @param scaleLabels Scale labels for y-variable (e.g., waiver(), scales::percent).
#' This value is passed to the \code{\link[ggplot2]{scale_y_continuous}} function.
#'
#' @return A ggplot2 plot object
#'
#' @family plot functions
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lemon facet_rep_wrap
#' @importFrom snakecase to_any_case
#'
#' @export
buildBarPlot <-
  function(df,
             x,
             y,
             fill = "none",
             facet = "none",
             title = NULL,
             facetScales = "fixed",
             scaleLabels = waiver()) {

    # Call function to calculate scaling
    groupVars <- c(x, facet)
    groupVars <- unique(groupVars[groupVars != "none"])
    df <- df %>% scaleVariable(y, groupVars)

    if (exists(paste0(y, "Avg"), df)) {
      df <- df %>%
        mutate(!!sym(paste0(y, "Avg")) := !!sym(paste0(y, "Avg")) / divisor)
    }

    # Add unit to y-labels
    scaleLabel <- case_when(
      df$divisor[1] == 1e3 ~ "\n(thousand)",
      df$divisor[1] == 1e6 ~ "\n(million)",
      TRUE ~ ""
    )
    yLabel <- paste(snakecase::to_any_case(y, "sentence"), scaleLabel)
    xLabel <- snakecase::to_any_case(x, "sentence")
    fillLabel <- snakecase::to_any_case(fill, "sentence")

    # Initialize plot
    if (fill != "none") {
      g <- ggplot(
        data = df,
        aes_string(
          x = x,
          y = paste0(y, "Scaled"),
          fill = fill
        )
      )
    } else {
      g <- ggplot(
        data = df,
        aes_string(
          x = x,
          y = paste0(y, "Scaled")
        )
      )
    }

    # Add bars
    g <- g + geom_col()

    # Add line and points for averages
    if (exists(paste0(y, "Avg"), df)) {
      g <- g +
        geom_hline(
          aes_string(yintercept = paste0(y, "Avg"), group = 1, linetype = "avgLab"),
          color = "#FF7900",
          size = 1.5
        )
    }

    # Add facets to plot
    if (facet != "none") {
      g <- g + lemon::facet_rep_wrap(
        as.formula(paste("~", facet)),
        ncol = 2,
        scales = facetScales,
        repeat.tick.labels = "all"
      )
    }

    # Add labels and color scales
    if (fill != "none") {
      g <- g + labs(x = xLabel, y = yLabel, fill = fillLabel, title = title) +
        scale_fill_brewer(palette = barAndLinePal(fill), na.value = "grey50") +
        scale_color_brewer(palette = barAndLinePal(fill), na.value = "grey50")
    } else {
      g <- g + labs(x = xLabel, y = yLabel, title = title)
    }

    # x-axis label angle
    xLabAngle <- if_else(max(nchar(as.character(df[[x]]))) >= 4, 45, 0)
    xHJust <- if_else(xLabAngle == 45, 1, 0.5)

    # Finalize plot
    g <- g +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = xLabAngle, hjust = xHJust),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        aspect.ratio = 1 / 2
      )

    # Set scale labels
    g <- g + scale_y_continuous(
      labels = scaleLabels,
      expand = expand_scale(mult = c(0, 0.05))
    )

    # Return plot
    return(g)
  }


#' Build map plot
#'
#' This function builds a choropleth plot given user specifications using ggplot2.
#'
#' The plot labels are based on variable names. The
#' \code{\link[snakecase]{to_any_case}} function is used to convert variable
#' names from the data frame in camel case to sentence case.
#'
#' If the fill variable is "permits" or "customers", then the data is log
#' transformed automatically.
#'
#' The y-axis labels are repeated for each
#' subplot using the \code{\link[lemon]{facet_rep_wrap}} function.
#'
#' @param df A data frame with plot data
#' @param y Name of fill variable (string)
#' @param facet Name of facet variable (string)
#' @param scaleLabels Scale labels for y-variable (e.g., waiver(), scales::percent).
#' This value is passed to the \code{\link[viridis]{scale_fill_viridis}} function.
#' @param title Title of plot (string)
#'
#' @return A ggplot2 plot object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lemon facet_rep_wrap
#' @importFrom snakecase to_any_case
#' @importFrom viridis scale_fill_viridis
#'
#' @export
buildMapPlot <-
  function(df,
             y,
             facet = "none",
             scaleLabels = waiver(),
             title = NULL) {
    fillLabel <- to_any_case(y, "sentence")
    if (y %in% c("items", "customers")) {
      df <-
        df %>%
        mutate(!!sym(y) := log10(!!sym(y) + 1))
      fillLabel <- paste0("log(", fillLabel, ")")
    }

    # Initialize plot
    g <- ggplot(
      data = df,
      mapping = aes_string(x = "long", y = "lat", group = "group", fill = y)
    ) +
      coord_fixed(1.3) +
      geom_polygon() +
      geom_polygon(color = "black")

    # Add color scales
    g <- g + viridis::scale_fill_viridis(labels = scaleLabels)

    g <- g + guides(fill = guide_colorbar(
      title = fillLabel,
      # barwidth = 2,
      # barheight = 20,
      nbins = 100
    ))

    # Add facets to plot
    if (facet != "none") {
      g <- g + lemon::facet_rep_wrap(
        as.formula(paste("~", facet)),
        ncol = 1,
        repeat.tick.labels = "all"
      )
    }

    # Finalize plot
    g <- g +
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      labs(title = title)


    # Return plot
    return(g)
  }


#' Build upset plot
#'
#' @import ggplot2
#' @importFrom gridExtra gtable_rbind
#' @importFrom egg ggarrange
#'
#' @export
buildUpsetPlot <-
  function(dfList, focusGroup = "none") {

    # Unpack data
    comboCount <- dfList$comboCount
    itemTypeCount <- dfList$itemTypeCount

    # Calculate scaling
    comboCount <- comboCount %>%
      mutate(divisor = case_when(
        (max(customers) %/% 1e6) > 0 ~ 1e6,
        (max(customers) %/% 1e3) > 0 ~ 1e3,
        TRUE ~ 1
      )) %>%
      mutate(customersScaled = customers / divisor)

    itemTypeCount <- itemTypeCount %>%
      mutate(customersScaled = customers / comboCount$divisor[1])

    # Add unit to y-labels
    scaleLabel <- case_when(
      comboCount$divisor[1] == 1e3 ~ "\n(thousand)",
      comboCount$divisor[1] == 1e6 ~ "\n(million)",
      TRUE ~ ""
    )
    yLabel <- paste(to_any_case("customers", "sentence"), scaleLabel)

    # Create matrix plot
    matrixPlot <-
      ggplot(
        comboCount,
        aes(x = groupID, y = itemType, group = groupID)
      ) +
      geom_point(alpha = 0.02, size = 10) +
      geom_point(data = . %>% filter(purchase == 1), size = 10) +
      geom_line(data = . %>% filter(purchase == 1), size = 2) +
      scale_color_identity() +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1 / 2
      ) +
      scale_x_discrete(expand = expand_scale(add = 0.45, mult = 0)) +
      guides(alpha = F) +
      labs(
        x = "Item combinations",
        y = "Item types"
      )

    # Create combo plot (top bar plot)
    comboPlot <-
      ggplot(comboCount) +
      geom_col(aes(x = groupID, y = customersScaled), width = 0.9, position = "dodge") +
      scale_x_discrete(expand = expand_scale(add = 0, mult = 0)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1 / 2
      ) +
      guides(fill = F) +
      labs(
        x = "Item combinations",
        y = yLabel
      )

    # Create permit type plot (side bar plot)
    permitTypePlot <-
      ggplot(itemTypeCount) +
      geom_col(aes(x = itemType, y = customersScaled), width = 0.9, position = "stack") +
      scale_x_discrete(expand = expand_scale(add = 0, mult = 0)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1 / 2
      ) +
      guides(fill = F) +
      coord_flip() +
      labs(
        x = "Item types",
        y = yLabel
      )

    # Create empty plot
    emptyPlot <-
      ggplot(itemTypeCount) +
      geom_blank() +
      scale_x_discrete(expand = expand_scale(add = 0, mult = 0)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_grey() +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1 / 2,
        panel.border = element_blank()
      ) +
      guides(fill = F) +
      coord_flip()

    # Build plot layout
    pdf(NULL)
    g <- egg::ggarrange(
      comboPlot, emptyPlot, matrixPlot, permitTypePlot,
      nrow = 2, ncol = 2, widths = c(3, 1.5), heights = c(1, 1),
      draw = FALSE
    )

    # Return plot
    return(g)
  }

#' Build radial sets plot
#'
#' @importFrom ggplotify as.ggplot
#' @importFrom RColorBrewer brewer.pal
#' @import circlize
#'
#' @export
buildRadialSetsPlot <- function(sharedCustomers,
                                degreeCount,
                                returnPlot = F,
                                linkThickness = "percent",
                                focusGroup = "none",
                                sectorLabelFontSize = 1.5,
                                axisLabelFontSize = 1,
                                maxLinkThickness = 15,
                                countScale = 1 / 1e3) {



  # Prepare plot data
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

  # Define color pallette
  myColors <- brewer.pal(
    length(itemTypes),
    barAndLinePal("itemTypes")
  )

  # Convert plot to object
  networkPlot <<- function() {

    # Initialize circos with 2 tracks
    circos.clear()
    circos.par(cell.padding = c(0.02, 0, 0.02, 0))

    # Track 1 - Sector labels
    circos.initialize(itemTypes,
      xlim = c(0, 1),
      sector.width = totalVec
    )

    # Track 2 - Histograms
    xlim <- matrix(c(rep(0, nTypes), totalVec),
      nrow = nTypes,
      ncol = 2
    )
    circos.initialize(itemTypes, xlim = xlim, sector.width = totalVec)

    # Plot track of sector names
    circos.trackPlotRegion(
      track.index = 1,
      bg.border = NA,
      ylim = c(0, 1),
      track.height = 0.2,
      panel.fun = function(x, y) {
        sector.index <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(mean(xlim),
          mean(ylim),
          sector.index,
          niceFacing = TRUE,
          cex = sectorLabelFontSize
        )
      }
    )

    # Plot track with bars for sum by degree (histograms)
    circos.trackPlotRegion(
      track.index = 2,
      ylim = c(0, maxDegree),
      track.height = 0.2,
      panel.fun = function(x, y) {
        sector.index <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")

        # Create axis
        circos.axis(labels.cex = axisLabelFontSize)

        # Index for current sector
        i <- which(itemTypes == sector.index)

        # Color for current sector
        sectorColor <- myColors[i]

        # Loop over bars for each degree
        y1 <- maxDegree
        for (j in c(1:maxDegree)) {

          # Length of bar divided by two
          dx <- degreeCountMat[i, j] / 2

          # Zero postion to center bar
          zero <- totalVec[i] / 2

          # Vector of x coordinates
          x1 <- seq(zero - dx, zero + dx, (2 * dx) / 1)

          # Vector of x coordinates out and back
          d1 <- c(x1, rev(x1))

          # Repeated y coordinates
          d2 <- c(
            rep(y1, length(x1)),
            rep(y1 - 1, length(x1))
          )

          # Draw bars of non-zero length
          if (dx != 0) {
            circos.polygon(d1, d2, col = sectorColor, border = "black")
          }
          y1 <- y1 - 1
        }
      }
    )

    # Focus edges
    linkColor <- "grey"
    if (focusGroup != "none") {
      linkColor <- myColors[which(itemTypes == focusGroup)]
    }

    # Draw links between sectors
    for (i in c(1:nTypes)) {
      for (j in c(1:nTypes)) {
        if (edges[i, j] != 0) {

          # Draw links
          circos.link(
            itemTypes[i],
            totalVec[i] / 2,
            itemTypes[j],
            totalVec[j] / 2,
            lwd = (edges[i, j] / maxWidth) * maxLinkThickness,
            col = linkColor
          )
        }
      }
    }

    if (linkThickness == "percent") {
      legendText <- paste0(seq(1, 0.25, -0.25) * maxWidth, "%")
    } else {
      legendText <- paste0(seq(1, 0.25, -0.25) * maxWidth * (1 / countScale))
    }
    # Lengend showing link thickness
    legend(
      "topleft",
      inset = 0.05,
      cex = sectorLabelFontSize,
      title = "Shared customers",
      lwd = maxLinkThickness * seq(1, 0.25, -0.25),
      col = linkColor,
      legend = legendText,
      bty = "n"
    )
  }

  networkPlot()

  # Convert plot to ggplot object
  gg <- ggplotify::as.ggplot(~ networkPlot(), scale = 1.1) +
    coord_equal()

  if (returnPlot) {
    return(gg)
  } else {
    return(NULL)
  }
}
