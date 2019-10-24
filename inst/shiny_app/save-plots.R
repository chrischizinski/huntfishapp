

# Module UI function ------------------------------------------------------
savePlotsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        2,
        uiOutput(ns("downloadUI"))
      ),
      column(
        2,
        uiOutput(ns("filenameUI"))
      ),
      column(
        4,
        selectInput(
          ns("plotTheme"),
          label = "Plot theme:",
          choices = c(
            "presentation (dark background)",
            "presentation (light background)",
            "document (dark background)",
            "document (light background)"
          ),
          selected = "document (light background)"
        )
      )
    ),
    fluidRow(
      hr(),
      strong("Advanced options:"),
      br(),
      br()
    ),
    fluidRow(
      column(
        3,
        numericInput(
          ns("fontSize"),
          label = "Font size:",
          value = 12
        )
      ),
      column(
        3,
        selectizeInput(
          ns("textColor"),
          label = "Text color:",
          choices = colors(distinct = T),
          selected = "black"
        )
      ),
      column(
        3,
        numericInput(
          ns("textAngle"),
          label = "Text label angle:",
          value = 45,
          min = -90,
          max = 90
        )
      ),
      column(
        3,
        selectizeInput(
          ns("bgColor"),
          label = "Background color:",
          choices = c("transparent", colors(distinct = T)),
          selected = "transparent"
        )
      )
    ),
    fluidRow(
      column(
        3,
        numericInput(
          ns("height"),
          label = "Height (inches):",
          value = 3.33,
          min = 1,
          max = 50
        )
      ),
      column(
        3,
        numericInput(
          ns("width"),
          label = "Width (inches):",
          value = 7,
          min = 1,
          max = 50
        )
      ),
      column(
        3,
        selectInput(
          ns("legendPosition"),
          label = "Legend position:",
          choices = c("none", "left", "right", "bottom", "top"),
          selected = "bottom"
        )
      ),
      column(
        3,
        checkboxInput(
          ns("includeTitle"),
          label = "include title",
          value = FALSE
        )
      )
    )
  )
}


# Module server function --------------------------------------------------

savePlots <- function(input, output, session, plotPromise, themeOverride = NULL, defaultFilename = "huntfishapp") {


  # Render filename input -------------------------------------------------
  output$filenameUI <- renderUI({
    req(defaultFilename)

    # Get namespace
    ns <- session$ns

    # Filename input
    textInput(
      ns("filename"),
      label = "Filename:",
      value = defaultFilename
    )
  })


  # Render download input -------------------------------------------------
  output$downloadUI <- renderUI({
    req(input$filename, myPlot())

    # Get namespace
    ns <- session$ns

    # Download button input
    downloadButton(
      ns("downloadPlot"),
      label = "Download Plot"
    )
  })

  # Update inputs conditional on premade plot selection -------------------
  observe({
    x <- input$plotTheme

    if (!is.null(x)) {
      if (x == "presentation (dark background)") {
        updateSelectizeInput(session, "textColor", selected = "white")
        updateSelectizeInput(session, "bgColor", selected = "transparent")
        updateNumericInput(session, "fontSize", value = 28)
        updateRadioButtons(session, "legendPosition", selected = "bottom")
        updateNumericInput(session, "textAngle", value = 45)
        updateNumericInput(session, "width", value = 12.5)
        updateNumericInput(session, "height", value = 5.5)
      } else if (x == "presentation (light background)") {
        updateSelectizeInput(session, "textColor", selected = "black")
        updateSelectizeInput(session, "bgColor", selected = "transparent")
        updateNumericInput(session, "fontSize", value = 28)
        updateRadioButtons(session, "legendPosition", selected = "bottom")
        updateNumericInput(session, "textAngle", value = 45)
        updateNumericInput(session, "width", value = 12.5)
        updateNumericInput(session, "height", value = 5.5)
      } else if (x == "document (dark background)") {
        updateSelectizeInput(session, "textColor", selected = "white")
        updateSelectizeInput(session, "bgColor", selected = "transparent")
        updateNumericInput(session, "fontSize", value = 12)
        updateRadioButtons(session, "legendPosition", selected = "bottom")
        updateNumericInput(session, "textAngle", value = 45)
        updateNumericInput(session, "width", value = 7)
        updateNumericInput(session, "height", value = 5)
      } else if (x == "document (light background)") {
        updateSelectizeInput(session, "textColor", selected = "black")
        updateSelectizeInput(session, "bgColor", selected = "transparent")
        updateNumericInput(session, "fontSize", value = 12)
        updateRadioButtons(session, "legendPosition", selected = "bottom")
        updateNumericInput(session, "textAngle", value = 45)
        updateNumericInput(session, "width", value = 7)
        updateNumericInput(session, "height", value = 5)
      }
    }
  })


  # Update plot -----------------------------------------------------------
  myPlot <- reactive({

    # Plot must be available
    plotPromise() %...>% (function(gg) {
      if (input$textAngle != 0) {
        textJust <- 1
      } else {
        textJust <- 0.5
      }
      
      # Unable to modify theme of
      if ("egg" %in% class(gg)) {
        return(gg)
      }
      
      gg <- gg +
        theme_bw(base_size = input$fontSize) +
        theme(
          text = element_text(color = input$textColor),
          strip.text = element_text(color = input$textColor),
          axis.text.x = element_text(angle = input$textAngle, hjust = textJust),
          axis.text = element_text(color = input$textColor),
          axis.ticks = element_line(color = input$textColor),
          axis.line = element_line(color = input$textColor),
          panel.grid = element_blank(),
          legend.position = input$legendPosition,
          panel.background = element_rect(fill = input$bgColor, color = NA),
          panel.border = element_blank(),
          plot.background = element_rect(fill = input$bgColor, color = NA),
          legend.title = element_blank(),
          legend.background = element_rect(fill = input$bgColor),
          legend.key = element_rect(fill = input$bgColor)
        )
      
      if (!input$includeTitle) {
        gg <- gg + ggtitle(NULL)
      }
      
      gg <- gg +
        themeOverride
      
      return(gg)
    })
  })


  # Download handler ------------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".png")
    },

    content = function(file) {

      # Set default bar color to light color
      if (grepl("dark", input$plotTheme)) {
        update_geom_defaults("bar", list(fill = "grey92"))
      }
      
      myPlot() %...>% (function(gg) {
        ggsave(
          file,
          plot = gg,
          device = "png",
          width = as.numeric(input$width),
          height = as.numeric(input$height),
          bg = "transparent"
        )
      })
    }
  )

  # Reset default bar color
  update_geom_defaults("bar", list(fill = "grey35"))
}
