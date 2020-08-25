# Module UI function ------------------------------------------------------
customersUI <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  # Tab panels
  tabsetPanel(
    type = "pills",
    # Description panel -------------------------------------------------------
    tabPanel(
      # Tab label
      "About the app",
      # Start fluid row
      includeHTML("descriptions/about_app.html")
    ),
    id = ns("activeTab")
  )
}
