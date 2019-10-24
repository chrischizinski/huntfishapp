# Module UI function -----------------------------------------------------
mapUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Tab panels
  tabsetPanel(
    type = "pills",
    # Plot panel ----------------------------------------------------------
    tabPanel(
      strong("Plot"),
      fluidRow(
        # Select variable for map
        column(
          3,
          selectInput(
            ns("yvar"),
            label = "Primary variable:",
            choices = c(
              "Items" = "items",
              "Customers" = "customers",
              "Churn Rate" = "churnRate",
              "Recruitment Rate" = "recruitmentRate",
              "Participation Rate" = "participationRate"
            ),
            selected = "participationRate"
          )
        ),
        column(
          3,
          selectInput(
            ns("mapRegion"),
            "Reporting region:",
            c(
              "County",
              "State"
            ),
            selected = "County"
          )
        ),
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
        # Select year for map
        column(
          3,
          uiOutput(ns("focusYearsUI"))
        )
      ),
      # Output plot UI
      uiOutput(ns("plotUI"))
    ),

    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "Description",
      includeHTML("descriptions/map.html")
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
map <- function(input, output, session, dataFilters, sharedInputs) {

  # Pause reactive --------------------------------------------------------
  observe({
    if ("map" %in% sharedInputs$activePanel()) {
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

  # Update focus year input based on y-variable ---------------------------
  observe({
    req(input$yvar)

    if (input$yvar == "churnRate") {
      updateSelectizeInput(
        session = session,
        inputId = "focusYears",
        choices = dataFilters()$itemYear[1]:(dataFilters()$itemYear[2] - 5)
      )
    }

    if (input$yvar == "recruitmentRate") {
      updateSelectizeInput(
        session = session,
        inputId = "focusYears",
        choices = (dataFilters()$itemYear[1] + 5):(dataFilters()$itemYear[2])
      )
    }
  })

  # Throttle focus year ---------------------------------------------------
  focusYear <- reactive({
    input$focusYears
  }) %>% debounce(1e3)

  # Define grouping variables ---------------------------------------------
  groupVars <- reactive({

    # Collect grouping variables
    if (input$mapRegion == "County") {
      groupVars <- c("state", "county", input$facet)
    } else {
      groupVars <- c("state", input$facet)
    }

    # Remove duplicate variables. Remove 'none' variables.
    groupVars <- unique(groupVars[groupVars != "none"])
  })

  # Filter data -----------------------------------------------------------
  updatedDataFilters <- reactive({
    req(focusYear())

    # Extract shared filters
    activeFilters <- dataFilters()

    # Filter data
    if (input$yvar %in% c("churnRate", "recruitmentRate")) {
      validate(
        need(
          (dataFilters()$itemYear[2] - dataFilters()$itemYear[1] + 1) >= 5,
          "Need 5 years of data. Change permit year range."
        )
      )

      # Permit stamp year filter
      if (input$yvar == "churnRate") {
        validate(
          need(
            (as.numeric(focusYear()) + 5) <= dataFilters()$itemYear[2],
            "Need 5 years of data."
          )
        )
        activeFilters[["itemYear"]] <-
          c(as.numeric(focusYear()), as.numeric(focusYear()) + 5)
      } else if (input$yvar == "recruitmentRate") {
        validate(
          need(
            (as.numeric(focusYear()) - 5) >= dataFilters()$itemYear[1],
            "Need 5 years of data."
          )
        )
        activeFilters[["itemYear"]] <-
          c(as.numeric(focusYear()) - 5, as.numeric(focusYear()))
      }
    } else if (input$yvar == "participationRate") {

      # Permit stamp year filter
      activeFilters[["itemYear"]] <- c(focusYear(), focusYear())

      # Resident filter
      activeFilters[["residency"]] <- "T"
    } else {

      # Permit stamp year filter
      activeFilters[["itemYear"]] <- c(focusYear(), focusYear())
    }

    if (input$mapRegion == "County") {

      # Permit stamp year filter
      activeFilters[["state"]] <- focusStateAbb
    }

    activeFilters
  })

  # Initialize query timer ------------------------------------------------
  queryTimer <- reactiveValues(start = NULL, stop = NULL, label = "Map")

  # Summarize data (reactive) ---------------------------------------------
  summaryData <- pauseableReactive({

    # Ensure values are available
    req(updatedDataFilters())

    # Start query timer
    queryTimer$start <- Sys.time()
    queryTimer$running <- TRUE
    message(paste(queryTimer$start, "starting SQL query -", queryTimer$label))

    # Show notification
    if (!isTRUE(getOption("shiny.testmode"))) {
      mapMsg <<- showNotification(
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
    yvar <- input$yvar
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

      # Call external functions to summarize data
      switch(
        yvar,
        "items" = countItems(permitData, groupVarsStatic),
        "customers" = countCustomers(permitData, groupVarsStatic),
        "churnRate" = calcChurn(permitData, groupVarsStatic),
        "recruitmentRate" = calcRecruitment(permitData, groupVarsStatic),
        "participationRate" = calcParticipation(
          permitData,
          c("itemYear", groupVarsStatic),
          pop = statePop %>% filter(state == focusStateAbb)
        )
      )
    }) %>% catch(function(reason) {
      showModal(genericError)
      removeNotification(req(mapMsg))
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

      removeNotification(req(mapMsg))

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

  # Plot data -------------------------------------------------------------
  plotData <- reactive({

    # Verify inputs are available
    req(
      summaryData(),
      focusYear(),
      input$yvar,
      input$facet
    )

    # Execute this code when SQL query is finished
    summaryData() %...>% (function(df) {

      # Check that variable is available
      validate(
        need(
          input$yvar %in% colnames(df),
          "Missing y-variable"
        ),
        need(nrow(df) > 0,
             "No data available")
      )

      # Load state and county polygons
      stateAbbLookup <- tibble(
        state = state.name,
        stateAbb = state.abb
      )
      states <- stateMap %>%
        mutate(state = as.character(state)) %>%
        left_join(stateAbbLookup, by = "state") %>%
        mutate(
          state = as.factor(state),
          stateAbb = as.factor(stateAbb)
        )
      lvlStates <- levels(states$stateAbb)
      counties <- countyMaps %>% filter(region == tolower(focusState))
      lvlCounties <- unique(counties$county)

      facet <- input$facet[input$facet != "none"]
      if (input$yvar %in% c("items", "customers")) {
        fillValue <- 0
      } else {
        fillValue <- NA
      }

      fillVars <- list()
      fillVars[[input$yvar]] <- fillValue
      if (input$mapRegion == "County") {
        vars <- c(facet, "county")
        dataComplete <-
          df %>%
          mutate(county = tolower(county)) %>%
          filter(state == focusStateAbb, county %in% lvlCounties) %>%
          mutate(county = factor(county, levels = lvlCounties)) %>%
          mutate_at(facet, factor) %>%
          complete(UQS(syms(vars)),
            fill = fillVars
          ) %>% 
          mutate_if(is.factor, as.character)
      } else {
        vars <- c(facet, "state")
        dataComplete <-
          df %>%
          filter(state %in% lvlStates) %>%
          mutate(state = factor(state, levels = lvlStates)) %>%
          mutate_at(facet, factor) %>%
          complete(UQS(syms(vars)),
            fill = fillVars
          ) %>% 
          mutate_if(is.factor, as.character)
      }

      # Join permit data to map shapes
      if (input$mapRegion == "County") {
        plotData <- counties %>% 
          mutate_if(is.factor, as.character) %>% 
          left_join(dataComplete, by = "county") %>%
          mutate(label = county)
      } else {
        plotData <- states %>% 
          mutate_if(is.factor, as.character) %>% 
          left_join(dataComplete, by = c("stateAbb" = "state")) %>%
          mutate(label = state)
      }

      plotData
    })
  })

  # Create ggplot (reactive) ----------------------------------------------
  gg <- reactive({
    # Verify dependents are available
    req(input$yvar)

    # Execute this code when SQL query is finished
    plotData() %...>%
      buildMapPlot(
        y = input$yvar,
        facet = input$facet,
        scaleLabels = if (grepl("Rate", input$yvar)) scales::percent else waiver(),
        title = waiver()
      )
  })

  # Render ggplot ---------------------------------------------------------
  output$GGPlot <- renderPlot({
    gg()
  })

  # Plot tooltip ----------------------------------------------------------
  output$hover_info <- renderUI({

    # Ensure hover input is available
    req(input$plot_hover)

    # Execute this code when SQL query is finished
    plotData() %...>%
      createMapTooltip(input$plot_hover)
  })

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({
    ns <- session$ns

    # Execute this code when SQL query is finished
    plotData() %...>%
      (function(df) {
        if (input$facet != "none") {
          nLevel <- length(unique(df[[input$facet]]))
        } else {
          nLevel <- 1
        }

        floor((
          sharedInputs$pageWidth() / 2.5
        ) * nLevel)
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
  # Override default plot theme
  themeOverride <-
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = "right"
    )
  callModule(savePlots, "spNS", gg, themeOverride, defaultFilename = "huntfishapp_map")
  
  # Call save data module -------------------------------------------------
  callModule(saveData, "saveData", summaryData, defaultFilename = "huntfishapp_map")
  
}
