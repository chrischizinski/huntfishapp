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
    )))
  ),

  # Main UI -------------------------------------------------------------
  mainUI("mainNS"),#end dashboardPage
  tags$footer("My footer", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
)
