# Module UI function ------------------------------------------------------
upsetUI <- function(id) {

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
          uiOutput(ns("nGroupsUI"))
        )
      ),
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      # Start fluid row
      includeHTML("descriptions/upset.html")
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
upset <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("upset" %in% sharedInputs$activePanel()) {
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
    label = "UpSet"
  )

  # Summarize data --------------------------------------------------------
  summaryData <- pauseableReactive({

    # Ensure values are available
    req(updatedDataFilters())

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      upsetMsg <<- showNotification(
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

      comboCount
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(upsetMsg))
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

      removeNotification(req(upsetMsg))

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



  # Render number of bars input -------------------------------------------
  output$nGroupsUI <- renderUI({

    # Get namespace
    ns <- session$ns

    req(summaryData())

    summaryData() %...>% (function(df) {
      df <- df %>%
        filter(customers > 0)

      # Focus year input
      numericInput(
        ns("nGroups"),
        label = "Number of Combos (bars):",
        min = 1,
        max = max(df$groupID),
        value = min(10, max(df$groupID)),
        step = 1
      )
    })
  })

  # Force output to update even when hidden
  outputOptions(output, "nGroupsUI", suspendWhenHidden = FALSE)

  # Prepare plot data -----------------------------------------------------
  plotData <- reactive({

    # Ensure values are available
    req(summaryData(), input$nGroups)

    # Execute this code when SQL query is finished
    summaryData() %...>% (function(comboCount) {
      
      validate(
        need(nrow(comboCount) > 0,
             "No data available")
      )

      # Calculate total customers
      itemTypeCount <-
        comboCount %>%
        filter(purchase == 1) %>%
        group_by(itemType) %>%
        summarize(customers = sum(customers)) %>%
        arrange(customers) %>%
        mutate(itemType = factor(itemType, levels = itemType)) %>%
        arrange(desc(customers))

      # Arrange factor levels by number of customers
      comboCount <-
        comboCount %>%
        mutate(
          itemType =
            factor(itemType,
              levels = levels(itemTypeCount$itemType)
            )
        ) %>%
        filter(groupID <= input$nGroups) %>%
        group_by(itemType) %>%
        filter(max(purchase) == 1) %>%
        ungroup()

      # Remove permit stamp types not in combo groups
      itemTypeCount <-
        itemTypeCount %>%
        filter(itemType %in% unique(comboCount$itemType))

      # Return list of data
      list(
        comboCount = comboCount,
        itemTypeCount = itemTypeCount
      )
    })
  })

  # Create ggplot ---------------------------------------------------------
  gg <- reactive({

    # Execute this code when SQL query is finished
    plotData() %...>%
      buildUpsetPlot()
  })


  # Render ggplot ---------------------------------------------------------
  output$GGPlot <- renderPlot({
    gg()
  }, bg = "transparent")

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({

    # Get namespace
    ns <- session$ns

    pageWidth <- isolate(sharedInputs$pageWidth())
    facet <- "none"

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
            height = paste0(h * 1.5, "px")
          )
        )
      })
  })
  
  # Call save plot module -------------------------------------------------
  callModule(savePlots, "spNS", gg, defaultFilename = "huntfishapp_upset")
  
  # Call save data module -------------------------------------------------
  callModule(saveData, "saveData", summaryData, defaultFilename = "huntfishapp_upset")
  
}
