context("Test plot interaction functions")


# createBarTooltip --------------------------------------------------------

# Hover list for testing
hover <-
  list(
    # Cursor position
    x = 1.1,
    y = 2.3,
    coords_css = list(x = 659L, y = 109L),
    coords_img = list(x = 659L, y = 109L),
    img_css_ratio = list(x = 1L, y = 1L),
    # Mapping of aesthetics to variables
    mapping = list(
      x = "xVar",
      y = "yVar"
    ),
    # Range of data in plot
    domain = list(
      left = 0.5,
      right = 8.5,
      bottom = 0L,
      top = 20
    ),
    # Range of data in pixels
    range = list(
      left = 0,
      right = 500,
      bottom = 0,
      top = 250
    ),
    log = list(x = NULL, y = NULL)
  )

# Plot data for testing
plotData <- tibble(
  xVar = factor(LETTERS[1:8]),
  yVar = seq(4, 18, length.out = 8)
)

# Helper function to extract tooltip display
extractDesc <- function(tooltip) {
  sub(".*<p>*(.*?) *</p>.*", "\\1", tooltip)
}

test_that("createBarTooltip: no fill or facet", {
  expect_equal(
    createBarTooltip(
      plotData = plotData,
      hover = hover
    ) %>%
      extractDesc(),
    "<b>X var</b>: A<br/><b>Y var</b>: 4"
  )
  expect_equal(
    createBarTooltip(
      plotData = plotData,
      hover = modifyList(hover, list(y = 5))
    ),
    NULL
  )
  expect_equal(
    createBarTooltip(
      plotData = plotData,
      hover = modifyList(hover, list(x = 1.9, y = 5))
    ) %>%
      extractDesc(),
    "<b>X var</b>: B<br/><b>Y var</b>: 6"
  )
})


# Plot data for testing
plotDataFill <- tibble(
  xVar = rep(factor(LETTERS[1:8]), 2),
  yVar = c(
    seq(4, 18, length.out = 8),
    seq(2, 9, length.out = 8)
  ),
  gender = factor(rep(c("Male", "Female"), each = 8))
)

test_that("createBarTooltip: no fill or facet", {
  expect_equal(
    createBarTooltip(
      plotData = plotDataFill,
      hover =
        modifyList(
          hover,
          list(mapping = list(fill = "gender"))
        )
    ) %>%
      extractDesc(),
    "<b>X var</b>: A<br/><b>Gender</b>: Male<br/><b>Y var</b>: 4"
  )
  expect_equal(
    createBarTooltip(
      plotData = plotDataFill,
      hover =
        modifyList(
          hover,
          list(
            y = 5,
            mapping = list(fill = "gender")
          )
        )
    ) %>%
      extractDesc(),
    "<b>X var</b>: A<br/><b>Gender</b>: Female<br/><b>Y var</b>: 2"
  )
  expect_equal(
    createBarTooltip(
      plotData = plotDataFill,
      hover =
        modifyList(
          hover,
          list(
            y = 7,
            mapping = list(fill = "gender")
          )
        )
    ),
    NULL
  )
})

hover <-
  list(
    # Cursor position
    x = 1.1,
    y = 10.1,
    coords_css = list(x = 659L, y = 109L),
    coords_img = list(x = 659L, y = 109L),
    img_css_ratio = list(x = 1L, y = 1L),
    # Cursor panel
    panelvar1 = "PanelVal",
    # Mapping of aesthetics to variables
    mapping = list(
      fill = "FillVar",
      x = "XVar",
      y = "YVar",
      panelvar1 = "PanelVar"
    ),
    # Range of data in plot
    domain = list(
      left = 0.5,
      right = 8.5,
      bottom = 0L,
      top = 20
    ),
    # Range of data in pixels
    range = list(
      left = 652.738301633193,
      right = 1141.80034358538,
      bottom = 285.43387978867,
      top = 40.9028588125753
    ),
    log = list(x = NULL, y = NULL)
  )

plotData <- tibble(
  x = factor(LETTERS[1:8]),
  y = seq(4, 18, length.out = 8)
)

test_that("createBarTooltip", {
  expect_equal(
    createBarTooltip(plotData = data_frame(), hover = list()),
    NULL
  )
})
