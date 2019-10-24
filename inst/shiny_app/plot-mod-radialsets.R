# Module UI function ------------------------------------------------------
radialsetsUI <- function(id) {

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
          uiOutput(ns("focusYearsUI"))
        ),
        column(
          3,
          uiOutput(ns("focusGroupUI"))
        ),
        column(
          3,
          # Link scaling input
          selectizeInput(
            ns("linkThickness"),
            label = "Link thickness:",
            choices = c(
              "Percent" = "percent",
              "Number of customers" = "customers"
            ),
            multiple = FALSE,
            selected = "percent"
          )
        )
      ),
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      # Start fluid row
      includeHTML("descriptions/radialsets.html")
    ),

    # Options panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Options",
      # Start fluid row
      fluidRow(
        NULL
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
radialsets <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("radialsets" %in% sharedInputs$activePanel()) {
      pause(summaryData, pause = FALSE)
    } else {
      pause(summaryData, pause = TRUE)
    }
  })

  # Render focus year input -----------------------------------------------
  output$focusYearsUI <- renderUI({

    # Must have less than 10 permit/stamp types
    cond <- length(dataFilters()[["itemType"]]) >= 2 &
      length(dataFilters()[["itemType"]]) <= 10
    validate(
      need(cond, "Please adjust data filters to include between 2 and 10 permit and/or stamp types")
    )

    # Get namespace
    ns <- session$ns

    # Focus year input
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


  # Render focus group input ---------------------------------------------
  output$focusGroupUI <- renderUI({
    req(input$focusYears)

    # Get namespace
    ns <- session$ns

    # Focus year input
    selectizeInput(
      ns("focusGroup"),
      label = "Focus item type:",
      choices = c("none", dataFilters()[["itemType"]]),
      multiple = FALSE,
      selected = "none"
    )
  })

  # Filter data -----------------------------------------------------------
  updatedDataFilters <- reactive({
    req(input$focusYears)

    # Extract shared filters
    activeFilters <- dataFilters()

    # Filer based on focus year
    activeFilters[["itemYear"]] <- c(input$focusYears, input$focusYears)

    # Return list of updated filters
    return(activeFilters)
  })


  # Initialize query timer ------------------------------------------------
  queryTimer <- reactiveValues(
    start = NULL,
    stop = NULL,
    label = "Radial Sets"
  )

  # Summarize data --------------------------------------------------------
  summaryData <- pauseableReactive({

    # Ensure values are available
    req(updatedDataFilters())

    # Data filters
    filters <- updatedDataFilters()

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      radsetsMsg <<- showNotification(
        paste0(
          "Running SQL query: ",
          queryTimer$label
        ),
        duration = NULL,
        type = "warning"
      )
    }

    # Get required reactive variables
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
      comboCount <- itemGroupCount(permitData)
      sharedCustomers <- countSharedCustomers(comboCount)
      degreeCount <- countItemTypes(comboCount)

      list(
        sharedCustomers = sharedCustomers,
        degreeCount = degreeCount
      )
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(radsetsMsg))
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

      removeNotification(req(radsetsMsg))

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
    req(summaryData())

    # Execute this code when SQL query is finished
    summaryData() %...>% (function(networkData) {
      
      networkData
    })
  })

  gg <- reactive({
    req(input$focusGroup)

    plotData() %...>% (function(networkData) {
      buildRadialSetsPlot(
        sharedCustomers = networkData$sharedCustomers,
        degreeCount = networkData$degreeCount,
        returnPlot = TRUE,
        focusGroup = input$focusGroup,
        linkThickness = input$linkThickness
      )
    })
  })

  # Render ggplot ---------------------------------------------------------
  output$GGPlot <- renderPlot({
    req(input$focusGroup)

    plotData() %...>% (function(networkData) {
      buildRadialSetsPlot(
        sharedCustomers = networkData$sharedCustomers,
        degreeCount = networkData$degreeCount,
        focusGroup = input$focusGroup,
        linkThickness = input$linkThickness
      )
    })
  }, bg = "transparent")

  # Plot tooltip ----------------------------------------------------------
  output$hover_info <- renderUI({

    # Ensure hover input is available
    req(input$plot_hover)

    # Execute this code when SQL query is finished
    plotData() %...>% (function(networkData) {
      createRadialsetsTooltip(
        sharedCustomers = networkData$sharedCustomers,
        degreeCount = networkData$degreeCount,
        input$plot_hover,
        focusGroup = input$focusGroup,
        linkThickness = input$linkThickness
      )
    })
  })

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({

    # Get namespace
    ns <- session$ns

    pageWidth <- isolate(sharedInputs$pageWidth())
    facet <- "none"

    # Execute this code when SQL query is finished
    plotData() %...>%
      (function(df) {
        sharedInputs$pageHeight() - 200
      }) %...>% (function(h) {
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
  callModule(savePlots, "spNS", gg, theme_void(), defaultFilename = "huntfishapp_radialsets")
  
  # Call save data module -------------------------------------------------
  df <- reactive({
    summaryData() %...>% (function(df) {
      df$sharedCustomers
    })
  })
  callModule(saveData, "saveData", df, defaultFilename = "huntfishapp_radialsets")

}
