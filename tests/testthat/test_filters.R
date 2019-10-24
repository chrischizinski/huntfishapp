context("Test data filtering")

# Filters -----------------------------------------------------

test_that("data filters: age", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(age = c(21, 21))) %>%
      distinct(age) %>%
      collect(n = Inf),
    tibble(age = 21)
  )
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(age = c(20, 35))) %>%
      distinct(age) %>%
      collect(n = Inf),
    tibble(age = as.numeric(20:35))
  )
})

test_that("data filters: county", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(county = c("lancaster"))) %>%
      distinct(county) %>%
      collect(n = Inf),
    tibble(county = "lancaster")
  )
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(county = c(
      "lancaster", "douglas"
    ))) %>%
      distinct(county) %>%
      collect(n = Inf),
    tibble(county = c("lancaster", "douglas"))
  )
})

test_that("data filters: gender", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(gender = "Male")) %>%
      distinct(gender) %>%
      collect(n = Inf),
    tibble(gender = "Male")
  )
})

test_that("data filters: issueDate", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(
      issueDate = c("2017-11-13", "2017-11-13")
    )) %>%
      mutate(
        year = year(issueDate),
        month = month(issueDate),
        day = day(issueDate)
      ) %>%
      distinct(year, month, day) %>%
      collect(n = Inf),
    tibble(
      year = 2017,
      month = 11,
      day = 13L
    )
  )
})

test_that("data filters: duration", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(duration = "Annual")) %>%
      distinct(duration) %>%
      collect(n = Inf),
    tibble(duration = "Annual")
  )
})

test_that("data filters: month", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(month = c(7, 7))) %>%
      distinct(month) %>%
      collect(n = Inf),
    tibble(month = 7)
  )
})

test_that("data filters: itemResidency", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(itemResidency = "T")) %>%
      distinct(itemResidency) %>%
      collect(n = Inf),
    tibble(itemResidency = c("T"))
  )
})

test_that("data filters: itemType", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(itemType = "Fish")) %>%
      distinct(itemType) %>%
      collect(n = Inf),
    tibble(itemType = "Fish")
  )
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(itemType = c("WaterFowl", "Deer"))) %>%
      distinct(itemType) %>%
      collect(n = Inf),
    tibble(itemType = c("Deer", "WaterFowl"))
  )
})

test_that("data filters: itemYear", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(itemYear = c(2017, 2017))) %>%
      distinct(itemYear) %>%
      collect(n = Inf),
    tibble(itemYear = 2017L)
  )
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(itemYear = c(2010, 2017))) %>%
      distinct(itemYear) %>%
      collect(n = Inf),
    tibble(itemYear = as.integer(2010:2017))
  )
})

test_that("data filters: residency", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(residency = "T")) %>%
      distinct(residency) %>%
      collect(n = Inf),
    tibble(residency = "T")
  )
})

test_that("data filters: state", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(state = "NE")) %>%
      distinct(state) %>%
      collect(n = Inf),
    tibble(state = "NE")
  )
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(state = c("NE", "CO"))) %>%
      distinct(state) %>%
      collect(n = Inf),
    tibble(state = c("NE", "CO"))
  )
})

test_that("data filters: year", {
  expect_equal(
    filterData(dataSource = "csv", activeFilters = list(year = c(2011, 2011))) %>%
      distinct(year) %>%
      collect(n = Inf),
    tibble(year = 2011)
  )
  expect_equal(
    filterData(
      dataSource = "csv",
      activeFilters =
        list(
          issueDate = c(NA, "2018-02-01"),
          itemYear = c(2017, 2017)
        )
    ) %>%
      distinct(year) %>%
      collect(n = Inf),
    tibble(year = as.numeric(2016:2018))
  )
})

# Messages ----------------------------------------------------

test_that("data messages", {
  expect_error(
    filterData(dataSource = "sql", conn = 1),
    "Input 'conn' should be Microsoft SQL Server connection."
  )
  expect_error(
    filterData(dataSource = "csv", activeFilters = 1),
    "Input 'activeFilters' should be a list."
  )
  expect_error(
    filterData(dataSource = "csv", activeFilters = list(itmType = "Fish")),
    "Unrecognized filter variables: itmType. Valid filter variables are: issueDate, itemResidency, duration, residency, state, county, gender, age, itemType, itemYear, ageGroup, month, year"
  )
})
