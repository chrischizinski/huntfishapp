library(maps)
library(maptools)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggmap)

# Nebraska county map
states <- maps::map("state", plot = F, fill = T)

# Convert to spatial polygon
statePolys <- map2SpatialPolygons(states,
  IDs = sapply(strsplit(states$names, ":"), "[", 1L),
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Get centroids
stateCentroids <- as_tibble(coordinates(statePolys), rownames = NA) %>%
  mutate(state = row.names(.)) %>%
  rename(longCent = V1, latCent = V2) %>%
  mutate(
    state = factor(toTitleCase(state)),
    longCent = as.numeric(longCent),
    latCent = as.numeric(latCent)
  )

# Save data
write_csv(stateCentroids, "data-raw/stateCentroids.csv")
usethis::use_data(stateCentroids, overwrite = TRUE)
