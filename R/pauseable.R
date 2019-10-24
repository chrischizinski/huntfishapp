# jcheng5
# https://gist.github.com/jcheng5/cc6647a4971b8125c1bb

#' Pauseable reactive
#'
#' Create a pauseable reactive. Code from jcheng5
#' https://gist.github.com/jcheng5/cc6647a4971b8125c1bb
#'
#' @param paused Indicates the starting state of the pauseable reactive.
#' @inheritParams shiny::reactive
#'
#' @export
pauseableReactive <- function(x, env = parent.frame(), quoted = FALSE,
                              priority = 0, domain = shiny::getDefaultReactiveDomain(), paused = TRUE) {
  shiny::installExprFunction(x, "func", eval.env = env, quoted = quoted)

  vals <- shiny::reactiveValues(value = NULL)
  obs <- shiny::observe(vals$value <- func(),
    priority = priority,
    domain = domain, suspended = paused
  )
  return(structure(
    reactive(vals$value),
    pauseObserver = obs
  ))
}

#' Pause a reactive
#'
#' Pause a pauseable reactive. Code from jcheng5
#' https://gist.github.com/jcheng5/cc6647a4971b8125c1bb
#'
#' @param pausableReactive A pauseable reactive
#' @param pause A logical indicative if paused
#'
#' @export
pause <- function(pausableReactive, pause = TRUE) {
  obs <- attr(pausableReactive, "pauseObserver", exact = TRUE)
  if (isTRUE(pause)) {
    obs$suspend()
  } else {
    obs$resume()
  }
}
