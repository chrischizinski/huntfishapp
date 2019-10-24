context("Test census API functions")

# Define key for census API
key <- keyring::key_get("censusAPIKey")

test_that("queryCensusAPI", {
  expect_equal(
    queryCensusAPI(
      data_url = "https://api.census.gov/data/2017/acs/acs5?",
      key = key,
      get = "B00001_001E",
      region = "for=state:31",
      numeric = TRUE
    ) %>%
      colnames(),
    c("B00001_001E", "state")
  )
  expect_equal(
    queryCensusAPI(
      data_url = "https://api.census.gov/data/2017/acs/acs5?",
      key = key,
      get = "B01001_002E,B01001_026E",
      region = "for=state:31",
      numeric = TRUE
    ) %>%
      colnames(),
    c("B01001_002E", "B01001_026E", "state")
  )
})

test_that("runCensusAPIQueries", {
  expect_equal(
    runCensusAPIQueries(
      data_url = "https://api.census.gov/data/2017/acs/acs5?",
      key = key,
      vars = "B00001_001E",
      region = "for=state:31",
      numeric = TRUE
    ) %>%
      colnames(),
    c("B00001_001E", "state")
  )
  expect_equal(
    runCensusAPIQueries(
      data_url = "https://api.census.gov/data/2017/acs/acs5?",
      key = key,
      vars = c("B01001_002E", "B01001_026E"),
      region = "for=state:31",
      numeric = TRUE
    ) %>%
      colnames(),
    c("B01001_002E", "B01001_026E", "state")
  )
  expect_setequal(
    runCensusAPIQueries(
      data_url = "https://api.census.gov/data/2017/acs/acs5?",
      key = key,
      vars = c(
        "B00001_001E", "B00002_001E",
        paste0("B01001_", str_pad(1:49, 3, pad = "0"), "E")
      ),
      region = "for=state:31",
      numeric = TRUE
    ) %>%
      colnames(),
    c(
      "state", "B00001_001E", "B00002_001E",
      paste0("B01001_", str_pad(1:49, 3, pad = "0"), "E")
    )
  )
})
