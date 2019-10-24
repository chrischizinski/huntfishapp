# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("churn")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "churn",
  output = "mainNS-churn-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set y-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-churn-yvar` = "churnRate",
  output = "mainNS-churn-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set fill variable -------------------------------------------------------
app$expectUpdate(
  `mainNS-churn-fill` = "residency",
  output = "mainNS-churn-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-churn-activeTab` = "Save data",
  output = "mainNS-churn-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-churn-activeTab` = "Description")
app$snapshot()
