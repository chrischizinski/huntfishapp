#  Module UI function ----------------------------------------------------
churnUI <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  # Tab panels
  tabsetPanel(
    type = "pills",
    # Plot panel ----------------------------------------------------------
    tabPanel(
      strong("Plot"),
      fluidRow(
        # Select variable
        column(
          3,
          selectInput(
            ns("yvar"),
            label = "Primary variable:",
            choices = c(
              "Churned customers" = "churnedCustomers",
              "Retained customers" = "retainedCustomers",
              "Churn rate" = "churnRate",
              "Retention rate" = "retentionRate"
            ),
            selected = "churnedCustomers"
          )
        ),
        column(
          3,
          # Sub-variable
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
        )
      ),
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      includeHTML("descriptions/churn.html")
    ),
    # Options panel -------------------------------------------------------
    tabPanel(
      "Options",
      fluidRow(
        # scales input
        column(
          2,
          radioButtons(
            ns("yScales"),
            label = "y-axis scales",
            choices = c(
              "start at zero" = "zero",
              "fit to data" = "auto",
              "manual" = "manual"
            ),
            selected = c("zero")
          )
        ),
        # ylim lower input
        column(2, uiOutput(ns("yLimUI"))),
        # window input
        column(
          2,
          uiOutput(ns("deathThresholdUI"))
        )
      )
    ),

    # Save plot panel -----------------------------------------------------
    tabPanel(
      "Save plot",
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
churn <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("churn" %in% sharedInputs$activePanel()) {
      pause(summaryData, pause = FALSE)
    } else {
      pause(summaryData, pause = TRUE)
    }
  })

  # Render year range slider ----------------------------------------------
  output$deathThresholdUI <- renderUI({

    # Validate required inputs
    validate(
      need(dataFilters()$itemYear, "Year filter undefined")
    )

    # Get namespace
    ns <- session$ns

    nYears <- dataFilters()$itemYear[2] -
      dataFilters()$itemYear[1] + 1

    sliderInput(
      ns("deathThreshold"),
      label = "Years without purchase:",
      min = 1,
      max = nYears - 1,
      value = max(1, min(5, nYears - 1))
    )
  })

  # Force output to update even when hidden
  outputOptions(output, "deathThresholdUI", suspendWhenHidden = FALSE)

  # Define grouping variables ---------------------------------------------
  groupVars <- reactive({

    # Grouping variables
    groupVars <- c(input$fill)

    # Remove duplicate variables. Remove 'none' variables.
    groupVars <- unique(groupVars[groupVars != "none"])
  })

  # Filter data -----------------------------------------------------------
  updatedDataFilters <- reactive({

    # Extract shared filters
    activeFilters <- dataFilters()

    return(activeFilters)
  })

  # Initialize query timer ------------------------------------------------
  queryTimer <- reactiveValues(start = NULL, stop = NULL, label = "Churn")

  # Summarize data (reactive) ---------------------------------------------
  summaryData <- pauseableReactive({
    
    req(input$fill, 
        input$deathThreshold)

    validate(
      need(
        (dataFilters()$itemYear[2] - dataFilters()$itemYear[1] + 1) >= 5,
        "Need 5 years of data. Change permit year range."
      )
    )

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      churnMsg <<- showNotification(
        paste0(
          "Running SQL query: ",
          queryTimer$label
        ),
        duration = NULL,
        type = "warning"
      )
    }

    # Get required reactive variables
    filters <- updatedDataFilters()
    groupVarsStatic <- groupVars()
    deathThreshold <- input$deathThreshold
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

      # Build query for permit table
      permitData <- filterData(
        dataSource = sharedInputs$dataSource,
        conn = conn,
        activeFilters = filters
      )

      # Summarize data (pull data from server)
      calcChurn(permitData, groupVarsStatic, deathThreshold) %>%
        mutate(itemYear = as.integer(as.character(itemYear)))
      
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(churnMsg))
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

      removeNotification(req(churnMsg))

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
    req(summaryData())

    # Execute this code when SQL query is finished
    summaryData() %...>% (function(df) {
      validate(
        need(all(groupVars() %in% colnames(df)),
             "Missing variables needed for plotting"),
        need(nrow(df) > 0,
             "No data available")
      )
      df
    })
  })

  # Render UI to set ylim -------------------------------------------------
  output$yLimUI <- renderUI({
    req(plotData())

    ns <- session$ns
    plotData() %...>% (function(df) {
      yrange <- signif(range(df[input$yvar], na.rm = TRUE), 2)

      sliderInput(
        ns("yLim"),
        label = "y-axes limits",
        min = 0,
        max = max(yrange) * 1.2,
        value = c(min(yrange) * 0.95, max(yrange) * 1.05)
      )
    })
  })
  
  # Show/hide y-limit input ------------------------------------------------
  observe({
    shinyjs::toggle("yLimUI", input$yScales == "manual")
  })

  # Force ylim output to update even when hidden
  outputOptions(output, "yLimUI", suspendWhenHidden = FALSE)


  # Create ggplot (reactive) ----------------------------------------------
  gg <- reactive({

    # Execute this code when SQL query is finished
    plotData() %...>% (function(df) {
      if (grepl("Rate", input$yvar)) {
        scaleLabels <- scales::percent
      } else {
        scaleLabels <- waiver()
      }

      # Call external function to build plot
      buildLinePlot(
        df = df,
        x = "itemYear",
        y = input$yvar,
        fill = input$fill,
        facet = "none",
        title = waiver(),
        facetScales = "fixed",
        yLimits = input$yLim,
        yScales = input$yScales,
        scaleLabels = scaleLabels
      )
    })
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
      createLineTooltip(input$plot_hover)
  })

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({
    # Get namespace
    ns <- session$ns

    # Execute this code when SQL query is finished
    plotData() %...>%
      calcualtePlotHeight(sharedInputs$pageWidth(), input$facet) %...>%

      (function(h) {
        # Extra div used ONLY to create positioned ancestor for tooltip
        div(
          style = "position:relative",
          # Plot output
          plotOutput(
            ns("GGPlot"),
            width = "100%",
            height = paste0(h, "px"),
            hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce")
          ),
          # Tooltip output
          uiOutput(ns("hover_info"))
        )
      })
  })

  # Call save plot module -------------------------------------------------
  callModule(savePlots, "spNS", gg, defaultFilename = "huntfishapp_churn")
  
  # Call save data module -------------------------------------------------
  callModule(saveData, "saveData", summaryData, defaultFilename = "huntfishapp_churn")
  
}
