library(maps)
library(maptools)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggmap)
library(snakecase)

states <- tolower(state.name)
states <- states[!states %in% c("alaska", "hawaii")]
countyCentroids <- NULL
for (state in states) {
  # County map
  county <- maps::map("county", state, plot = F, fill = T)

  # Convert to spatial polygon
  countyPolys <- map2SpatialPolygons(
    county,
    IDs = sapply(strsplit(county$names, ","), "[", 2L),
    proj4string = CRS("+proj=longlat +datum=WGS84")
  )

  # Get centroids
  stateCountyCentroids <-
    as_tibble(coordinates(countyPolys), rownames = NA) %>%
    mutate(state = state, county = row.names(.)) %>%
    rename(longCent = V1, latCent = V2) %>%
    mutate(
      county = factor(county),
      longCent = as.numeric(longCent),
      latCent = as.numeric(latCent)
    )

  countyCentroids <- rbind(countyCentroids, stateCountyCentroids)
}

# Save data
write_csv(countyCentroids, "data-raw/countyCentroids.csv")
usethis::use_data(countyCentroids, overwrite = TRUE)
