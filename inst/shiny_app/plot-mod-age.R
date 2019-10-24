# Module UI function ------------------------------------------------------
ageUI <- function(id) {

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
              "Duration" = "duration"
            ),
            selected = "none"
          )
        ),
        column(
          3,
          uiOutput(ns("focusYearsUI"))
        ),
        column(
          3,
          numericInput(
            ns("binSize"),
            label = "Bin size:",
            value = 5,
            min = 1,
            max = 100
          )
        )
      ),
      # Output plot UI
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      # Start fluid row
      includeHTML("descriptions/age.html")
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
            selected = c("fixed")
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
age <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("age" %in% sharedInputs$activePanel()) {
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

  # Define grouping variables ---------------------------------------------
  groupVars <- reactive({

    # Grouping variables
    groupVars <- input$facet

    # Remove duplicate variables. Remove 'none' variables.
    groupVars <- unique(groupVars[groupVars != "none"])
  })

  # Filter data -----------------------------------------------------------
  updatedDataFilters <- reactive({
    req(input$focusYears)

    # Extract shared filters
    activeFilters <- dataFilters()

    # Filer based on focus year
    activeFilters[["itemYear"]] <- c(input$focusYears, input$focusYears)

    # Filter based on age range
    activeFilters[["age"]] <- c(0, 100)

    # Return list of updated filters
    return(activeFilters)
  })


  # Throttle bin size -----------------------------------------------------
  binSize <- reactive({
    input$binSize
  }) %>% debounce(1e3)

  # Initialize query timer ------------------------------------------------
  queryTimer <- reactiveValues(start = NULL, stop = NULL, label = "Age")

  # Summarize data --------------------------------------------------------
  summaryData <- pauseableReactive({

    # Ensure values are available
    req(updatedDataFilters(),
        binSize())

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      ageMsg <<- showNotification(
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
    bins <- binSize()
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

      permitData %>%
        distinct(UQS(syms(c("customerUID", "age", groupVarsStatic)))) %>%
        collect(n = Inf) %>%
        mutate(ageBin = ageCut(age, lower = 0, upper = 85, by = bins)) %>%
        countCustomers(c("ageBin", groupVarsStatic))
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(ageMsg))
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

      removeNotification(req(ageMsg))

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
      binSize(),
      input$facet
    )

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

  # Create ggplot ---------------------------------------------------------
  gg <- reactive({

    # Execute this code when SQL query is finished
    plotData() %...>%
      buildBarPlot(
        x = "ageBin",
        y = "customers",
        fill = "none",
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
  callModule(savePlots, "spNS", gg, defaultFilename = "huntfishapp_age")
  
  # Call save data module -------------------------------------------------
  callModule(saveData, "saveData", summaryData, defaultFilename = "huntfishapp_age")
  
}
