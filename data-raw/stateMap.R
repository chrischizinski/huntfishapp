library(maps)
library(maptools)
library(dplyr)
library(readr)
library(ggmap)

# Load centroids data
stateCentroids <- read_csv("data-raw/stateCentroids.csv")

# State map
stateMap <- map_data("state") %>%
  mutate(state = factor(toTitleCase(region))) %>%
  left_join(stateCentroids) %>%
  as_tibble()

# Save data
write_csv(stateMap, "data-raw/stateMap.csv")
usethis::use_data(stateMap, overwrite = TRUE)
