library(maps)
library(maptools)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggmap)
library(snakecase)

# Load centroids data
countyCentroids <- read_csv("data-raw/countyCentroids.csv")

# County map
countyMaps <- map_data("county") %>%
  mutate(county = subregion) %>%
  left_join(countyCentroids,
    by = c("county", "region" = "state")
  ) %>%
  as_tibble()

# Save data
write_csv(countyMaps, "data-raw/countyMaps.csv")
usethis::use_data(countyMaps, overwrite = TRUE)
