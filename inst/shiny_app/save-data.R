

# Module UI function ------------------------------------------------------
saveDataUI <- function(id) {
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
      )
    ),
    fluidRow(
      DT::DTOutput(ns("summaryTable"), width = "80%")
    )
  )
}


# Module server function --------------------------------------------------

saveData <- function(input, output, session, dataPromise, defaultFilename = "huntfishapp") {

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
    req(input$filename)

    # Get namespace
    ns <- session$ns

    # Download button input
    downloadButton(
      ns("downloadData"),
      label = "Download Data"
    )
  })


  # Render data table -----------------------------------------------------
  output$summaryTable <- DT::renderDT({
    dataPromise() %...>% (function(df) {return(df)})
  })


  # Download handler ------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      dataPromise() %...>% 
        write.csv(file, row.names = FALSE)
    }
  )
}
