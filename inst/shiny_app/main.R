# Module UI function ------------------------------------------------------
mainUI <- function(id) {

  # Create a namespace function using the provided id ---------------------
  ns <- NS(id)

  fluidPage( # Top panel -----------------------------------------------------------
    div(
      id = "rcorners5",

      fluidRow(
        column(
          2,
          align = "left",
          img(
            src = "duck-hunter.gif",
            alt = "duck hunter",
            height = "100"
          )
        ),
        column(
          6,
          align = "center",
          h2("huntfishapp: Exploratory Data Analysis App for Hunting, Fishing, and Outdoor Recreation")
        ),
        column(
          4,
          align = "right",
          img(
            src = "fish_hunt_logo_no_text_crop.png",
            alt = "fish hunt logo",
            height = "100"
          )
        )
      )
    ),

    # Filter sidebar --------------------------------------------------
    sidebarLayout(
      sidebarPanel(
        bsplus::bs_modal(
          id = "modalFilters",
          title = "Filters Sidebar",
          body = includeHTML("descriptions/filters.html"),
          size = "medium"
        ),
        div(style = "display: inline; float: right;",
        bsplus::shiny_iconlink("info-circle", "fa-2x") %>%
          bsplus::bs_attach_modal(id_modal = "modalFilters")),
        h4(HTML("Item Attributes")),
        selectizeInput(
          ns("itemType"),
          label = "Item Type(s):",
          choices = itemType,
          selected = itemTypeDefault,
          multiple = TRUE,
          width = "100%"
        ),
        checkboxGroupInput(
          ns("itemResidency"),
          label = "Item Residency:",
          choices = c(
            "Resident Item" = "T",
            "Non-resident Item" = "F"
          ),
          selected = itemResidencyDefault
        ),
        selectizeInput(
          ns("duration"),
          label = "Item Duration(s):",
          choices = duration,
          selected = durationDefault,
          multiple = TRUE,
          width = "100%"
        ),
        hr(),
        h4(HTML("Customer Demographics")),
        sliderInput(
          ns("age"),
          label = "Age:",
          min = 0,
          max = 100,
          value = ageDefault,
          step = 1
        ),
        checkboxGroupInput(
          ns("gender"),
          label = "Gender:",
          choices = c("Female", "Male"),
          selected = genderDefault
        ),
        hr(),
        h4(HTML("Purchase Date")),
        sliderInput(
          ns("itemYear"),
          label = "Item Year:",
          min = itemYear[1],
          max = itemYear[2],
          value = itemYearDefault,
          step = 1,
          sep = ""
        ),
        hr(),
        h4(HTML("Customer Address")),
        checkboxGroupInput(
          ns("residency"),
          label = "Residency:",
          choices = c("Resident" = "T", "Non-resident" = "F"),
          selected = residencyDefault
        ),
        selectizeInput(
          ns("county"),
          label = "County of Residence:",
          choices = county,
          selected = countyResidencyDefault,
          multiple = T
        ),
        width = 2
      ),
      mainPanel(

        # Filter description -------------------------------------------
        htmlOutput(ns("filterDesc")),

        # Hidden input active panel (used with shinytest) --------------
        if (isTRUE(getOption("shiny.testmode"))) {
          conditionalPanel(condition = "false",
                           selectInput(
                             ns("activePanelSelect"),
                             label = "Active Panel:",
                             choices = c(
                               "",
                               "revenue",
                               "customers",
                               "gender",
                               "age",
                               "map",
                               "recruitment",
                               "churn",
                               "upset",
                               "radialsets",
                               "about"
                             ),
                             selected = ""
                           ))
        },
        shinyBS::bsCollapse(
          id = ns("collapsePlotModules"),

          # Plot modules -----------------------------------------------
          shinyBS::bsCollapsePanel("Revenue",
            value = "revenue",
            revenueUI(ns("revenue"))
          ),
          shinyBS::bsCollapsePanel("Customers",
            value = "customers",
            customersUI(ns("customers"))
          ),
          shinyBS::bsCollapsePanel("Gender Ratio",
            value = "gender",
            genderUI(ns("gender"))
          ),
          shinyBS::bsCollapsePanel("Age Distribution",
            value = "age",
            ageUI(ns("age"))
          ),
          shinyBS::bsCollapsePanel("Map",
            value = "map",
            mapUI(ns("map"))
          ),
          shinyBS::bsCollapsePanel("Recruitment",
            value = "recruitment",
            recruitmentUI(ns("recruitment"))
          ),
          shinyBS::bsCollapsePanel("Churn",
            value = "churn",
            churnUI(ns("churn"))
          ),
          shinyBS::bsCollapsePanel("Item Combinations (UpSet plot)",
            value = "upset",
            upsetUI(ns("upset"))
          ),
          shinyBS::bsCollapsePanel("Item Combinations (Radial Sets plot)",
            value = "radialsets",
            radialsetsUI(ns("radialsets"))
          ),
          shinyBS::bsCollapsePanel("About the app",
                                   value = "about",
                                   aboutUI(ns("about"))
          )
        ),
        width = 10
      )
    )
  )
}


# Module server function --------------------------------------------------
main <- function(input, output, session, sharedInputs) {

  # Define namespace ------------------------------------------------------
  ns <- session$ns

  # Define data filters ---------------------------------------------------
  dataFilters <- reactive({

    # Create list of filters (NULL values are dropped)
    activeFilters <- list()
    activeFilters$itemType <- input$itemType
    activeFilters$duration <- input$duration
    activeFilters$itemResidency <- input$itemResidency
    activeFilters$gender <- input$gender
    activeFilters$age <- if (all(input$age %in% c(0, 100))) {
      NULL
    } else {
      input$age
    }
    activeFilters$itemYear <- input$itemYear
    activeFilters$residency <- input$residency
    activeFilters$county <- input$county

    # Return data
    return(activeFilters)
  }) %>% debounce(2e3)


  # Create filter description ---------------------------------------------
  output$filterDesc <- renderText({

    # Filters must be available
    req(dataFilters())

    activeFilters <- dataFilters()

    # Replace T and F with descriptions
    if (!is.null(activeFilters[["itemResidency"]])) {
      activeFilters[["itemResidency"]] <-
        stringr::str_replace(activeFilters[["itemResidency"]], "T", "Resident Item")
      activeFilters[["itemResidency"]] <-
        stringr::str_replace(activeFilters[["itemResidency"]], "F", "Nonresident Item")
    }
    if (!is.null(activeFilters[["residency"]])) {
      activeFilters[["residency"]] <-
        stringr::str_replace(activeFilters[["residency"]], "T", "Resident")
      activeFilters[["residency"]] <-
        stringr::str_replace(activeFilters[["residency"]], "F", "Nonresident")
    }

    # Convert filter list to string
    desc <- paste(lapply(
      activeFilters,
      FUN = function(s)
        paste(s, collapse = ", ")
    ), collapse = " | ")

    paste('<p style="padding: 1em 0 1em 0;"><font color="black"><b>Active Filters:</b></font>', desc, "</p>")
  })

  # Active panel ----------------------------------------------------------
  activePanel <- reactive({
    input$collapsePlotModules
  })

  sharedInputs$activePanel <- activePanel

  # Update active panel (used with shinytest) -------------------------------
  observe({
    shinyBS::updateCollapse(session,
      "collapsePlotModules",
      open = input$activePanelSelect
    )
  })

  # Call shiny modules ----------------------------------------------------
  callModule(revenue, "revenue", dataFilters, sharedInputs)
  callModule(customers, "customers", dataFilters, sharedInputs)
  callModule(gender, "gender", dataFilters, sharedInputs)
  callModule(age, "age", dataFilters, sharedInputs)
  callModule(map, "map", dataFilters, sharedInputs)
  callModule(recruitment, "recruitment", dataFilters, sharedInputs)
  callModule(churn, "churn", dataFilters, sharedInputs)
  callModule(upset, "upset", dataFilters, sharedInputs)
  callModule(radialsets, "radialsets", dataFilters, sharedInputs)
  callModule(about, "about") #
}
