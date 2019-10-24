# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("gender")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "gender",
  output = "mainNS-gender-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-gender-facet` = "itemType",
  output = "mainNS-gender-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set x-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-gender-xvar` = "monthYear",
  output = "mainNS-gender-GGPlot",
  timeout = 10e3
)
app$snapshot()
app$expectUpdate(
  `mainNS-gender-focusMonths` = "Jul",
  `mainNS-gender-xvar` = "issueDate",
  output = "mainNS-gender-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-gender-activeTab` = "Save data",
  output = "mainNS-gender-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-gender-activeTab` = "Description")
app$snapshot()