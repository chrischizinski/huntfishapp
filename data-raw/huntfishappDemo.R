library(dplyr)
library(readr)
library(lubridate)

# Connect to database
conn <- DBI::dbConnect(odbc::odbc(),
  dsn = "PermitDatabase",
  database = "NGPCPermits201904"
)

# Load some filtered data
filteredData <-
  tbl(conn, "huntfishapp") %>%
  filter(
    itemType %in% c(
      "Fish",
      "Deer",
      "Hunt",
      "Hunt Fish Combo",
      "Spring Turkey",
      "Fall Turkey",
      "WaterFowl"
    ),
    itemYear >= 2010,
    itemYear <= 2018
  ) %>%
  collect(n = Inf)

# Take a random sample of customers
customerOrig <-
  filteredData %>%
  distinct(customerUID) %>%
  sample_n(10e3)

# Anonymize customer data
customerAnon <- 
  filteredData %>%
  inner_join(customerOrig) %>% 
  mutate(birthYear = year(issueDate) - age) %>% 
  distinct(customerUID, birthYear, gender, county, state, residency) %>% 
  # Remove customerUID
  arrange(customerUID) %>% 
  mutate(customerUID_new = row_number()) %>%
  arrange(customerUID_new) %>% 
  # County post randomization 
  mutate(
    county_old = county,
    county = as.character(sdcMicro::pram(as.factor(county))$x_pram)
  ) %>% 
  # Add noise to birth year
  mutate(
    birthYear_old = birthYear,
    birthYear = birthYear + round(rnorm(n(), 0, 2.5))
  ) %>% 
  # Gender post randomization
  mutate(
    gender_old = gender,
    gender = as.character(sdcMicro::pram(as.factor(gender))$x_pram)
  ) %>% 
  # Drop old variables
  select(-county_old, -birthYear_old, -gender_old) %>% 
  # Drop unchanged variables
  select(-residency, -state)

# Sample items by customer
huntfishappOrig <-
  filteredData %>%
  # Drop customer variables
  select(-county, -age, -gender) %>% 
  inner_join(customerAnon,
             by = c("customerUID" = "customerUID")) %>% 
  # Drop old customer ID
  select(-customerUID) %>% 
  rename(customerUID = customerUID_new)

# Obfuscate data
huntfishappDemo <-
  huntfishappOrig %>%
  arrange(issueDate) %>%
  # Add new itemUID
  mutate(itemUID = row_number()) %>%
  # Add noise to issue date
  mutate(
    issueDate_old = issueDate,
    issueDate = issueDate + round(rnorm(1, 0, 7))
  ) %>%
  # Recalculate age
  mutate(
    age = year(issueDate) - birthYear
  ) %>%
  # Drop old variables
  select(-issueDate_old) %>% 
  # Sort data
  arrange(customerUID)

# Save data
write_csv(huntfishappDemo, "data-raw/huntfishappDemo.csv")
usethis::use_data(huntfishappDemo, overwrite = TRUE)
