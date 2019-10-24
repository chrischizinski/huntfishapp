# Module UI function ------------------------------------------------------
revenueUI <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  # Tab panels
  tabsetPanel(
    type = "pills",

    # Plot panel ----------------------------------------------------------
    tabPanel(
      # Tab label
      strong("Plot"),
      fluidRow(
        column(
          3,
          # Sub-variable
          selectInput(
            ns("facet"),
            label = "Subplot variable:",
            choices = c(
              "Total" = "none",
              "Item Type" = "itemType",
              "Gender" = "gender",
              "Residency" = "residency",
              "Item Residency" = "itemResidency",
              "Duration" = "duration",
              "Age Group" = "ageGroup"
            ),
            selected = "none"
          )
        ),
        column(
          3,
          selectInput(
            ns("fill"),
            label = "Color variable:",
            choices = c(
              "Total" = "none",
              "Item Type" = "itemType",
              "Gender" = "gender",
              "Residency" = "residency",
              "Item Residency" = "itemResidency",
              "Duration" = "duration",
              "Age Group" = "ageGroup"
            ),
            selected = "none"
          )
        ),
        column(
          3,
          # Reporting period
          selectInput(
            ns("xvar"),
            label = "Reporting period:",
            choices = c(
              "Annually (item year)" = "itemYear",
              "Monthly" = "monthYear",
              "Daily" = "issueDate"
            ),
            selected = "itemYear"
          )
        ),
        column(
          3,
          # Focus year input
          uiOutput(ns("focusYearsUI"))
        ),
        column(
          3,
          # Focus month input
          selectInput(
            ns("focusMonths"),
            label = "Focus month:",
            choices = month.abb
          )
        )
      ),
      # Output plot UI
      helpText("Click a bar to zoom in. Double click to zoom out."),
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      # Start fluid row
      includeHTML("descriptions/revenue.html")
      # includeMarkdown("descriptions/revenue.md")
    ),

    # Options panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Options",
      # Start fluid row
      fluidRow(
        # y-scales input
        column(
          2,
          radioButtons(
            ns("facetScales"),
            label = "Subplot y-scales:",
            choices = c(
              "same scales" = "fixed",
              "different scales " = "free_y"
            ),
            selected = c("same")
          )
        )
      )
    ),

    # Save plot panel -----------------------------------------------------
    tabPanel(
      # Tab label
      "Save plot",
      # Output save plot UI
      savePlotsUI(ns("spNS"))
    ),

    # Save data panel -----------------------------------------------------
    tabPanel(
      # Tab label
      "Save data",
      # Output save data UI
      saveDataUI(ns("saveData"))
    ),
    id = ns("activeTab")
  )
}


# Module server function --------------------------------------------------
revenue <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("revenue" %in% sharedInputs$activePanel()) {
      pause(summaryData, pause = FALSE)
    } else {
      pause(summaryData, pause = TRUE)
    }
  })

  # Render focus year input -----------------------------------------------
  output$focusYearsUI <- renderUI({
    
    # Validate required inputs
    validate(
      need(dataFilters()$itemYear, "Year filter undefined")
    )
    
    # Get namespace
    ns <- session$ns
    
    selectizeInput(
      ns("focusYears"),
      label = "Focus year:",
      choices = dataFilters()$itemYear[1]:dataFilters()$itemYear[2],
      multiple = FALSE,
      selected = dataFilters()$itemYear[2]
    )
    
  })
  
  # Force output to update even when hidden
  outputOptions(output, "focusYearsUI", suspendWhenHidden = FALSE)
  
  # Show/hide focus year input ---------------------------------------------
  observe({
    shinyjs::toggle(id = "focusYearsUI",
           condition = input$xvar %in% c("monthYear", "issueDate"))
  })

  # Show/hide focus month input -------------------------------------------
  observe({
    shinyjs::toggle(id = "focusMonths",
           condition = input$xvar == "issueDate")
  })

  # Define grouping variables ---------------------------------------------
  groupVars <- reactive({

    # Grouping variables
    groupVars <- c(input$xvar, input$facet, input$fill)

    # Remove duplicate variables. Remove 'none' variables.
    groupVars <- unique(groupVars[groupVars != "none"])
  })

  # Filter data -----------------------------------------------------------
  updatedDataFilters <- reactive({

    # Extract shared filters
    activeFilters <- dataFilters()
    
    # Require x-variable
    req(input$xvar)

    # Switch for x-variable
    if (input$xvar == "monthYear") {
      # Ensure input focus year is available
      req(input$focusYears)

      # Define issue date range based on focus year
      endDate <- paste0(as.numeric(input$focusYears) + 1, "-", "01-31")
      startDate <- paste0(as.numeric(input$focusYears) - 1, "-", "11-01")

      # Update filter values
      activeFilters[["issueDate"]] <- c(startDate, endDate)
      activeFilters[["itemYear"]] <- c(input$focusYears, input$focusYears)
    } else if (input$xvar == "issueDate") {

      # Ensure input focus year/month are available
      req(input$focusYears, input$focusMonths)

      # Update filter conditions
      monthInt <- which(month.abb == input$focusMonths)
      activeFilters[["month"]] <- c(monthInt, monthInt)
      activeFilters[["year"]] <- c(input$focusYears, input$focusYears)
    } else if (input$xvar == "itemYear") {

      # Nothing to do here
    } else {

      # Error catch
      warning("x-variable not recognized")
    }

    # Return list of updated filters
    return(activeFilters)
  })


  # Initialize query timer ------------------------------------------------
  queryTimer <- reactiveValues(
    start = NULL,
    stop = NULL,
    label = "Revenue"
  )

  # Summarize data --------------------------------------------------------
  summaryData <- pauseableReactive({

    # Ensure values are available
    req(
      updatedDataFilters(),
      input$xvar,
      input$facet
    )

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      revenueMsg <<- showNotification(
        paste0(
          "Running SQL query: ",
          queryTimer$label
        ),
        duration = NULL,
        type = "warning"
      )
    }

    # Get required reactive variables
    fill <- input$fill
    xvar <- input$xvar
    facet <- input$facet
    filters <- updatedDataFilters()
    groupVarsStatic <- groupVars()
    DSN <- sharedInputs$DSN
    UID <- sharedInputs$UID
    PWD <- sharedInputs$PWD

    # Create a future for SQL query evaluation
    future({

      # Create SQL connnection
      if (sharedInputs$dataSource == "sql") {
        conn <-
          DBI::dbConnect(
            odbc::odbc(),
            dsn = DSN,
            uid = UID,
            pwd = PWD,
            MultiSubnetFailover = "No"
          )
        on.exit(DBI::dbDisconnect(conn))
      }

      # Define grouping variables for SQL server
      groupVarsSQL <- groupVarsStatic
      if ("monthYear" %in% groupVarsSQL) {
        groupVarsSQL <- groupVarsSQL[groupVarsSQL != "monthYear"]
        groupVarsSQL <- c(groupVarsSQL, "month", "year")
      }

      # Build query for permit table
      permitData <- filterData(
        dataSource = sharedInputs$dataSource,
        conn = conn,
        activeFilters = filters
      )

      # Summarize data (pull data from server)
      totalRevenue <- sumRevenue(permitData, groupVarsSQL)
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(revenueMsg))
    })
  })

  # Stop query timer ------------------------------------------------------
  observeEvent(summaryData(), {
    queryTimer$stop <- Sys.time()
    queryTimer$elapsed <-
      round(queryTimer$stop - queryTimer$start)

    if (!isTRUE(getOption("shiny.testmode"))) {
      message(
        paste0(
          queryTimer$stop,
          " completed SQL query - ",
          queryTimer$label,
          " (",
          queryTimer$elapsed,
          " secs)"
        )
      )

      removeNotification(req(revenueMsg))

      showNotification(
        paste0(
          "Completed SQL query: ",
          queryTimer$label,
          " (",
          queryTimer$elapsed,
          " secs)"
        ),
        duration = 5,
        type = "message"
      )
    }
  })

  # Prepare plot data -----------------------------------------------------
  plotData <- reactive({

    # Ensure values are available
    req(
      summaryData(),
      input$facet
    )

    # Execute this code when SQL query is finished
    summaryData() %...>% (function(df) {
      finalData <- df
      
      validate(
        need(all(groupVars() %in% colnames(finalData)),
             "Missing variables needed for plotting"),
        need(nrow(df) > 0,
             "No data available")
        )

      # Calculate average
      if (input$xvar == "itemYear") {
        # Define time window for average calculation
        maxYear <- dataFilters()$itemYear[2]
        minYear <- dataFilters()$itemYear[1]
        startYear <- max(maxYear - 5, minYear)
        stopYear <- maxYear - 1

        # Calculate average over subset of years
        groupVarsAvg <-
          groupVars()[!groupVars() %in% c(input$fill, "itemYear")]
        avg <- calcAvgValue(
          finalData,
          "revenue",
          startYear,
          stopYear,
          groupVarsAvg
        )

        # Join average to count data
        if (length(groupVarsAvg) == 0) {
          finalData <- cbind(finalData, avg)
        } else {
          finalData <-
            finalData %>%
            left_join(avg, by = groupVarsAvg)
        }
      }

      # Return final data
      finalData
    })
  })

  # Create ggplot ---------------------------------------------------------
  gg <- reactive({

    # Ensure x-variable input is available
    req(input$xvar)

    # Execute this code when SQL query is finished
    plotData() %...>%
      buildBarPlot(
        x = input$xvar,
        y = "revenue",
        fill = input$fill,
        facet = input$facet,
        title = waiver(),
        facetScales = input$facetScales,
        scaleLabels = waiver()
      )
  })


  # Render ggplot ---------------------------------------------------------
  output$GGPlot <- renderPlot({
    gg()
  }, bg = "transparent")

  # Plot tooltip ----------------------------------------------------------
  output$hover_info <- renderUI({

    # Ensure hover input is available
    req(input$plot_hover)

    # Execute this code when SQL query is finished
    plotData() %...>%
      createBarTooltip(input$plot_hover)
  })

  # Plot click actions ----------------------------------------------------
  observeEvent(input$plotClick, {

    # Execute this code when SQL query is finished
    plotData() %...>% (function(plotData) {

      # Get click attributes
      click <- input$plotClick

      # Name of x-variable
      x <- click$mapping$x

      # Get x-variable value
      if (is.factor(plotData[[x]])) {
        xVal <- levels(plotData[[x]])[round(click$x)]
      } else if (is.Date(plotData[[x]])) {
        xVal <- as.Date(click$x, origin = "1970-01-01")
      }

      # Update inputs based on click
      if (x == "itemYear") {
        # Set x-variable input to monthYear
        updateSelectizeInput(
          session = session,
          inputId = "xvar",
          selected = "monthYear"
        )

        # Set focus year input to clicked value
        updateSelectizeInput(
          session = session,
          inputId = "focusYears",
          selected = xVal
        )
      } else if (x == "monthYear") {
        # Set x-variable input to issueDate
        updateSelectizeInput(
          session = session,
          inputId = "xvar",
          selected = "issueDate"
        )

        # Set focus year input to clicked value
        updateSelectizeInput(
          session = session,
          inputId = "focusYears",
          selected = paste0("20", substr(xVal, 5, 6))
        )

        # Set focus month input to clicked value
        updateSelectizeInput(
          session = session,
          inputId = "focusMonths",
          selected = substr(xVal, 1, 3)
        )
      }
    })
  })


  # Plot double-click actions ---------------------------------------------
  observeEvent(input$plotDblClick, {

    # Execute this code when SQL query is finished
    plotData() %...>% (function(plotData) {

      # Get click attributes
      click <- input$plotDblClick

      # Name of x-variable
      x <- click$mapping$x

      # Update inputs based on double click
      if (x == "monthYear") {
        # Set x-variable input to itemYear
        updateSelectizeInput(
          session = session,
          inputId = "xvar",
          selected = "itemYear"
        )
      } else if (x == "issueDate") {
        # Set x-variable input to monthYear
        updateSelectizeInput(
          session = session,
          inputId = "xvar",
          selected = "monthYear"
        )
      }
    })
  })

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({

    # Get namespace
    ns <- session$ns

    pageWidth <- isolate(sharedInputs$pageWidth())
    facet <- isolate(input$facet)

    # Execute this code when SQL query is finished
    plotData() %...>%
      calcualtePlotHeight(pageWidth, facet) %...>%

      (function(h) {
        # Extra div used ONLY to create positioned ancestor for tooltip
        div(
          style = "position:relative",
          # Plot output
          plotOutput(
            ns("GGPlot"),
            width = "100%",
            height = paste0(h, "px"),
            click = ns("plotClick"),
            dblclick = ns("plotDblClick"),
            hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce")
          ),
          # Tooltip output
          uiOutput(ns("hover_info"))
        )
      })
  })

  # Call save plot module -------------------------------------------------
  callModule(savePlots, "spNS", gg, defaultFilename = "huntfishapp_revenue")

  # Call save data module -------------------------------------------------
  callModule(saveData, "saveData", summaryData, defaultFilename = "huntfishapp_revenue")
  
}
