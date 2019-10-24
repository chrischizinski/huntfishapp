# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("radialsets")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c(
  "Fish", "Deer", "Hunt", "Hunt Fish Combo", "Spring Turkey", "Fall Turkey",
  "WaterFowl"
))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "radialsets",
  output = "mainNS-radialsets-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set focus variable -------------------------------------------------------
app$expectUpdate(
  `mainNS-radialsets-focusGroup` = "Deer",
  output = "mainNS-radialsets-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set link thickness ------------------------------------------------------
app$expectUpdate(
  `mainNS-radialsets-linkThickness` = "customers",
  output = "mainNS-radialsets-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-radialsets-activeTab` = "Save data",
  output = "mainNS-radialsets-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-radialsets-activeTab` = "Description")
app$snapshot()