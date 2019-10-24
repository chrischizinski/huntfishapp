#' Launches the shiny app
#'
#' @return shiny application object
#'
#' @examples
#' \dontrun{
#' launchApp()
#' }
#'
#' @export launchApp
launchApp <- function() {
  appDir <- system.file("shiny_app", package = "huntfishapp")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `huntfishapp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
