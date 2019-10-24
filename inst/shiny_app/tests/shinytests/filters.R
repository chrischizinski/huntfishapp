# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("filters")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "revenue",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)

# Test item filter --------------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-facet` = "itemType",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-itemType` = "Fish",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Test item residency filter -------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-facet` = "itemResidency",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-itemResidency` = "T",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Test duration filter -------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-fill` = "duration",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-duration` = "Annual",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Test age filter ---------------------------------------------------------

app$expectUpdate(
  `mainNS-revenue-fill` = "ageGroup",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-age` = c(0, 34),
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Test gender filter ------------------------------------------------------

app$expectUpdate(
  `mainNS-revenue-fill` = "gender",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-gender` = "Male",
  output = "mainNS-revenue-GGPlot",
  timeout = 15e3
)
app$snapshot()

# Test year filter --------------------------------------------------------

app$expectUpdate(
  `mainNS-itemYear` = c(2010, 2016),
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()


# Test residency filter ---------------------------------------------------

app$expectUpdate(
  `mainNS-revenue-fill` = "residency",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-residency` = "T",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()


# Test county filter ------------------------------------------------------

app$expectUpdate(
  `mainNS-activePanelSelect` = "map",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

app$expectUpdate(
  `mainNS-county` = "lancaster",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()
