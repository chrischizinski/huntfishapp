# Server function ---------------------------------------------------------
function(input, output, session) {

  # Reactive page dimensions ----------------------------------------------
  pageWidth <- reactive({
    input$dimension[1]
  })

  pageHeight <- reactive({
    input$dimension[2]
  })
  
  # Modal dialog for password input ---------------------------------------
  dataModal <- function(failed = FALSE) {
    modalDialog(
      radioButtons("dataSource",
                   "Data source:",
                   choices = c(
                     "SQL Database" = "sql",
                     "Sample Data" = "csv"
                   ),
                   selected = "sql",
                   width = "100%"
      ),
      conditionalPanel(
        condition = "input.dataSource == 'sql'",
        textInput("usernameText", "SQL Username:"),
        passwordInput("passwordText", "SQL Password:")
      ),
      if (failed) {
        div(tags$b("Invalid password", style = "color: red;"))
      },
      footer = tagList(
        actionButton("passwordSubmit", "Continue")
      ),
      size = "s"
    )
  }
  
  # Show modal when button is clicked.
  showModal(dataModal())
  
  # Remove modal dialog ---------------------------------------------------
  observeEvent(input$passwordSubmit, ignoreInit = T, {
    if (passwordCheck() == "valid" | input$dataSource == "csv") {
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })

  # Update password when button is clicked --------------------------------
  login <- eventReactive(input$passwordSubmit, ignoreNULL = F, {
    list(
      username = input$usernameText,
      password = input$passwordText
    )
  })

  # Validate password -----------------------------------------------------
  passwordCheck <- reactive({
    req(login())

    # Try to connect to database
    result <- tryCatch({
      conn <- DBI::dbConnect(odbc::odbc(),
        dsn = DSN,
        uid = login()$username,
        pwd = login()$password,
        MultiSubnetFailover = "No"
      )
      tbl(conn, "huntfishapp") %>%
        collect(n = 1)
      DBI::dbDisconnect(conn)
      "valid"
    }, warning = function(w) {
      "invalid"
    }, error = function(e) {
      "invalid"
    })

    return(result)
  })

  # Call main module ------------------------------------------------------
  observeEvent(login(), ignoreInit = T, {

    # Call main module
    if (passwordCheck() == "valid" | input$dataSource == "csv") {
      sharedInputs <- list()
      sharedInputs$pageWidth <- pageWidth
      sharedInputs$pageHeight <- pageHeight
      sharedInputs$dataSource <- input$dataSource
      sharedInputs$DSN <- DSN
      sharedInputs$UID <- login()$username
      sharedInputs$PWD <- login()$password
      callModule(main, "mainNS", sharedInputs)
    }
  })
}
