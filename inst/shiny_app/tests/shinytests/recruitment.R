# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("recruitment")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "recruitment",
  output = "mainNS-recruitment-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set y-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-recruitment-yvar` = "recruitmentRate",
  output = "mainNS-recruitment-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set fill variable -------------------------------------------------------
app$expectUpdate(
  `mainNS-recruitment-fill` = "residency",
  output = "mainNS-recruitment-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-recruitment-activeTab` = "Save data",
  output = "mainNS-recruitment-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-recruitment-activeTab` = "Description")
app$snapshot()
