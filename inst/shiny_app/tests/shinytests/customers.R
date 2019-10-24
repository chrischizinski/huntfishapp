# Setup test --------------------------------------------------------------
app <- ShinyDriver$new("../../", loadTimeout = 20e3, seed = 4323)
app$setWindowSize(width = 1366, height = 768)
app$snapshotInit("customers")

# Initialize app ----------------------------------------------------------
app$setInputs(dataSource = "csv")
app$setInputs(passwordSubmit = "click")
app$setInputs(`mainNS-itemType` = c("Fish", "Deer"))
Sys.sleep(5)

# Expand panel ------------------------------------------------------------
app$expectUpdate(
  `mainNS-activePanelSelect` = "customers",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot variable ----------------------------------------------------
app$expectUpdate(
  `mainNS-customers-facet` = "itemType",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set x-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-customers-xvar` = "monthYear",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()
app$expectUpdate(
  `mainNS-customers-focusMonths` = "Jul",
  `mainNS-customers-xvar` = "issueDate",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set subplot scales ------------------------------------------------------
app$setInputs(`mainNS-customers-activeTab` = "Options")
app$setInputs(`mainNS-customers-activeTab` = "<strong>Plot</strong>")
app$expectUpdate(
  `mainNS-customers-facetScales` = "free_y",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set y-variable ----------------------------------------------------------
app$expectUpdate(
  `mainNS-customers-yvar` = "items",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Set fill variable -------------------------------------------------------
app$expectUpdate(
  `mainNS-customers-fill` = "residency",
  output = "mainNS-customers-GGPlot",
  timeout = 10e3
)
app$snapshot()

# Preview data ------------------------------------------------------------
app$expectUpdate(
  `mainNS-customers-activeTab` = "Save data",
  output = "mainNS-customers-saveData-summaryTable",
  timeout = 10e3
)
app$snapshot()

# View plot description ---------------------------------------------------
app$setInputs(`mainNS-customers-activeTab` = "Description")
app$snapshot()