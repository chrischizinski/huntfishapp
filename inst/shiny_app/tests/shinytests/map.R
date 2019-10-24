# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("map")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "map",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-map-facet` = "itemType",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set y-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-map-yvar` = "churnRate",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set map region ----------------------------------------------------------
app$expectUpdate(
  `mainNS-map-mapRegion` = "State",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set primary variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-map-yvar` = "participationRate",
  output = "mainNS-map-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-map-activeTab` = "Save data",
  output = "mainNS-map-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-map-activeTab` = "Description")
app$snapshot()