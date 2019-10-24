library(huntfishapp)
# Setup -------------------------------------------------------------------

# State
focusState <- "Nebraska"
focusStateAbb <- state.abb[tolower(state.name) == tolower(focusState)]

# Data source name for ODBC connection
DSN <- "license_data"

# Default levels
itemType <- c(
  "Duplicate Park Entry", "Aquatic", "Fish", "Hunt", "Hunter Ed Firearm",
  "Paddlefish Archery", "Temporary Federal Duck", "Elk", "Habitat",
  "Park", "Certificate", "Fall Turkey", "Hunter Ed Archery", "Spring Turkey",
  "Antelope", "Hunt Fish Combo", "Multi-Species", "Paddlefish Snagging",
  "Mountain Lion", "WaterFowl", "Aquatic Invasive Species", "Bighorn Sheep",
  "Deer", "Fur Harvest"
)
duration <- c(
  "Annual", "Lifetime (age 0-16)", "Lifetime (age 46+)", "3-Day",
  "Daily", "Lifetime (age 16-45)", "Lifetime (age 6-15)", "Season",
  "3 Year", "Lifetime (age 0-5)", "Lifetime Legacy", "1-Day", "Lifetime (age 0-15)",
  "5 Year", "Lifetime", "2-Day", "Federal Fiscal", "Lifetime (age 17+)"
)
itemYear <- c(2006, 2019)
data("countyMaps", package = "huntfishapp")
county <- unique(countyMaps[countyMaps$region == tolower(focusState), ]$county)

# Default filters (NULL for no filters)
itemTypeDefault <-
  c(
    "Fish", "Deer", "Hunt", "Hunt Fish Combo", "Spring Turkey", "Fall Turkey",
    "WaterFowl"
  )
itemYearDefault <- c(2010, 2018)
durationDefault <- NULL
itemResidencyDefault <- NULL
ageDefault <- c(0, 100)
genderDefault <- NULL
residencyDefault <- NULL
countyResidencyDefault <- NULL

# Set plan for how futures are resolved -----------------------------------
future::plan("multiprocess")
# future::plan("sequential")

# Set default ggplot theme ------------------------------------------------
myTheme <- ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = "grey92"),
    axis.line = ggplot2::element_line(colour = "black")
  )
ggplot2::theme_set(myTheme)

# Set default bar color
ggplot2::update_geom_defaults("bar", list(fill = "grey35"))

# Source modules ----------------------------------------------------------
source("main.R")
source("save-plots.R")
source("save-data.R")
source("plot-mod-revenue.R")
source("plot-mod-customers.R")
source("plot-mod-gender.R")
source("plot-mod-age.R")
source("plot-mod-map.R")
source("plot-mod-recruitment.R")
source("plot-mod-churn.R")
source("plot-mod-upset.R")
source("plot-mod-radialsets.R")


# Generic error message ---------------------------------------------------

genericError <- modalDialog(
  title = HTML(
    '<span class="text-danger"><i class="fa fa-bomb fa-1x">
                       </i>&nbsp;<strong>Error</strong></span>'
  ),
  "An error has occurred. Check your logs or contact the app author for clarification.",
  footer = modalButton("Dismiss"),
  size = "s"
)
