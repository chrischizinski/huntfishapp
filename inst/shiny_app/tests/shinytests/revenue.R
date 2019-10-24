# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("revenue")

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
app$snapshot()

# Set subplot variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-facet` = "itemType",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set fill variable -------------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-fill` = "residency",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set x-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-xvar` = "monthYear",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()
app$expectUpdate(
  `mainNS-revenue-focusMonths` = "Jul",
  `mainNS-revenue-xvar` = "issueDate",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot scales ------------------------------------------------------
app$setInputs(`mainNS-revenue-activeTab` = "Options")
app$setInputs(`mainNS-revenue-activeTab` = "<strong>Plot</strong>")
app$expectUpdate(
  `mainNS-revenue-facetScales` = "free_y",
  output = "mainNS-revenue-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-revenue-activeTab` = "Save data",
  output = "mainNS-revenue-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

