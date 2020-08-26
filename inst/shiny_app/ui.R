fluidPage(
  
  # Use shinyjs
  shinyjs::useShinyjs(),

  # Javascript and CSS files ----------------------------------------------
  tagList(
    tags$head(singleton(tags$script(src = "windowSize.js"))),
    tags$head(singleton(tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    ))),

  # Main UI -------------------------------------------------------------
  mainUI("mainNS")
  )
)