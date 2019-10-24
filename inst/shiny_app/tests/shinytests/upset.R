# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("upset")

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
  `mainNS-activePanelSelect` = "upset",
  output = "mainNS-upset-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-upset-activeTab` = "Save data",
  output = "mainNS-upset-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-upset-activeTab` = "Description")
app$snapshot()