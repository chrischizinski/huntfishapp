# Load libraries
library(readr)
library(forcats)
library(snakecase)

# Define key for census API
key <- rstudioapi::askForSecret("censusKey")

stateFIPS <- str_pad(as.character(state.fips$fips), 2, pad = "0")
statePop <- NULL
for (state in stateFIPS) {
  # Pull data from Census API
  censusData <- runCensusAPIQueries(
    data_url = "https://api.census.gov/data/2017/acs/acs5?",
    key = key,
    vars = paste0("B01001_", str_pad(2:49, 3, pad = "0"), "E"),
    region = paste0("for=county:*&in=state:", state)
  )


  # Define data frame with census age ranges for variables
  ageRange <- data_frame(
    variable = paste0("B01001_", str_pad(2:49, 3, pad = "0"), "E"),
    minAge = rep(c(0, 0, 5, 10, 15, 18, 20, 21, 22, 25, 30, 35, 40, 45, 50, 55, 60, 62, 65, 67, 70, 75, 80, 85), 2),
    maxAge = rep(c(Inf, 4, 9, 14, 17, 19, 20, 21, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 66, 69, 74, 79, 84, Inf), 2)
  )

  # Gather census data and add variables for gender and age range
  censusDataLabeled <-
    censusData %>%
    gather(variable, population, -state, -county) %>%
    mutate(gender = if_else(grepl(paste(paste0("B01001_", str_pad(2:25, 3, pad = "0"), "E"), collapse = "|"), variable), "Male", "Female")) %>%
    left_join(ageRange, by = "variable") %>%
    mutate(label = if_else(grepl("B01001_002E|B01001_026E", variable), "total", paste0(minAge, "-", maxAge))) %>%
    rename(stateFP = state, countyFP = county)

  # Merge smaller age bins into 5-year bins
  censusDataSummary <-
    censusDataLabeled %>%
    filter(label != "total") %>%
    mutate(ageGroup = case_when(
      label %in% c("0-4", "5-9", "10-14", "15-17") ~ "0-17",
      label %in% c("18-19", "20-20", "21-21", "22-24") ~ "18-24",
      label %in% c("25-29", "30-34") ~ "25-34",
      label %in% c("35-39", "40-44") ~ "35-44",
      label %in% c("45-49", "50-54") ~ "45-54",
      label %in% c("55-59", "60-61", "62-64") ~ "55-64",
      TRUE ~ "65+"
    )) %>%
    mutate(
      ageGroup = factor(ageGroup),
      ageGroup = fct_reorder(ageGroup, minAge)
    ) %>%
    group_by(stateFP, countyFP, gender, ageGroup) %>%
    summarize(population = sum(population)) %>%
    ungroup()

  # Add county and state names
  statePopulation <-
    censusDataSummary %>%
    left_join(state.fips %>%
      mutate(fips = str_pad(as.character(fips), 2, pad = "0")) %>%
      select(stateFP = fips, state = abb),
    by = "stateFP"
    ) %>%
    left_join(county.fips %>%
      mutate(fips = str_pad(as.character(fips), 5, pad = "0")) %>%
      mutate(
        stateFP = substr(as.character(fips), 1, 2),
        countyFP = substr(as.character(fips), 3, 5)
      ) %>%
      separate(polyname, c(NA, "county"), sep = ",") %>%
      select(stateFP, countyFP, county),
    by = c("stateFP", "countyFP")
    ) %>%
    mutate(
      gender = as.factor(gender),
      county = as.factor(county),
      state = as.factor(state)
    )

  # Bind data
  statePop <- rbind(statePop, statePopulation)
}

# Save data
write_csv(statePop, "data-raw/statePop.csv")
usethis::use_data(statePop, overwrite = TRUE)
