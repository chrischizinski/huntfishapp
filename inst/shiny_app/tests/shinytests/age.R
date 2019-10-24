# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("age")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "age",
  output = "mainNS-age-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-age-facet` = "itemType",
  output = "mainNS-age-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot scales ------------------------------------------------------
app$setInputs(`mainNS-age-activeTab` = "Options")
app$setInputs(`mainNS-age-activeTab` = "<strong>Plot</strong>")
app$expectUpdate(
  `mainNS-age-facetScales` = "free_y",
  output = "mainNS-age-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-age-activeTab` = "Save data",
  output = "mainNS-age-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-age-activeTab` = "Description")
app$snapshot()
