context("Test analysis functions")

# scaleVariable -----------------------------------------------------------

test_that("scaleVariable", {
  expect_equal(
    scaleVariable(df = tibble(revenue = seq(1, 10)), y = "revenue"),
    tibble(
      revenue = seq(1, 10),
      divisor = 1,
      revenueScaled = seq(1, 10) / 1
    )
  )
  expect_equal(
    scaleVariable(df = tibble(revenue = seq(1e3, 10e3, 1e3)), y = "revenue"),
    tibble(
      revenue = seq(1e3, 10e3, 1e3),
      divisor = 1e3,
      revenueScaled = seq(1e3, 10e3, 1e3) / 1e3
    )
  )
  expect_equal(
    scaleVariable(df = tibble(revenue = seq(1e6, 10e6, 1e6)), y = "revenue"),
    tibble(
      revenue = seq(1e6, 10e6, 1e6),
      divisor = 1e6,
      revenueScaled = seq(1e6, 10e6, 1e6) / 1e6
    )
  )
  expect_equal(
    scaleVariable(
      df = tibble(
        revenue = rep(seq(1, 10), 2),
        gender = rep(c("Male", "Female"), each = 10)
      ),
      y = "revenue"
    ),
    tibble(
      revenue = rep(seq(1, 10), 2),
      gender = rep(c("Male", "Female"), each = 10),
      divisor = 1,
      revenueScaled = rep(seq(1, 10), 2) / 1
    )
  )
  expect_equal(
    scaleVariable(
      df = tibble(
        revenue = rep(seq(1e3, 10e3, 1e3), 2),
        gender = rep(c("Male", "Female"), each = 10)
      ),
      y = "revenue"
    ),
    tibble(
      revenue = rep(seq(1e3, 10e3, 1e3), 2),
      gender = rep(c("Male", "Female"), each = 10),
      divisor = 1e3,
      revenueScaled = rep(seq(1e3, 10e3, 1e3), 2) / 1e3
    )
  )
  expect_equal(
    scaleVariable(
      df = tibble(
        revenue = rep(seq(1e6, 10e6, 1e6), 2),
        gender = rep(c("Male", "Female"), each = 10)
      ),
      y = "revenue"
    ),
    tibble(
      revenue = rep(seq(1e6, 10e6, 1e6), 2),
      gender = rep(c("Male", "Female"), each = 10),
      divisor = 1e6,
      revenueScaled = rep(seq(1e6, 10e6, 1e6), 2) / 1e6
    )
  )
})


# ageCut ------------------------------------------------------------------

test_that("ageCut", {
  expect_equal(
    ageCut(
      x = 0:100, lower = 0, upper = 90, by = 10L,
      sep = "-", above.char = "+"
    ),
    factor(c(
      rep("0-9", 10),
      rep("10-19", 10),
      rep("20-29", 10),
      rep("30-39", 10),
      rep("40-49", 10),
      rep("50-59", 10),
      rep("60-69", 10),
      rep("70-79", 10),
      rep("80-89", 10),
      rep("90+", 11)
    ))
  )
  expect_equal(
    ageCut(
      x = 0:100, lower = 0, upper = 85, by = 10,
      sep = "-", above.char = "+"
    ),
    factor(c(
      rep("0-9", 10),
      rep("10-19", 10),
      rep("20-29", 10),
      rep("30-39", 10),
      rep("40-49", 10),
      rep("50-59", 10),
      rep("60-69", 10),
      rep("70-79", 10),
      rep("80-89", 10),
      rep("90+", 11)
    ))
  )
  expect_equal(
    ageCut(
      x = 0:100, lower = 0, upper = 85, by = 1,
      sep = "-", above.char = "+"
    ),
    factor(c(
      seq(0, 84),
      rep("85+", 16)
    ), levels = c(seq(0, 84), "85+"))
  )
})



# countCustomers -------------------------------------------------------

test_that("countCustomers", {
  expect_equal(
    tibble(
      customerUID = 1:10
    ) %>%
      countCustomers(),
    tibble(
      customers = 10L
    )
  )
  expect_equal(
    tibble(
      customerUID = rep(1:10, 2),
      gender = rep(c("Male", "Female"), each = 10)
    ) %>%
      countCustomers("gender"),
    tibble(
      gender = factor(c("Male", "Female")),
      customers = c(10L, 10L)
    )
  )
})

test_that("countCustomers messages", {
  expect_error(
    countCustomers(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    countCustomers(data.frame(x = 1)),
    "Required variables not present in data frame: customerUID"
  )
  expect_error(
    countCustomers(data.frame(customerUID = 1), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    countCustomers(data.frame(customerUID = 1), "gender"),
    "Grouping variables not present in data frame: gender"
  )
  expect_error(
    countCustomers(data.frame(customerUID = 1), c("age", "gender")),
    "Grouping variables not present in data frame: age, gender"
  )
  expect_error(
    countCustomers(data.frame(customerUID = 1, gender = "Male"), c("age", "gender")),
    "Grouping variables not present in data frame: age"
  )
})


# calcGenderProportion ----------------------------------------------------

test_that("calcGenderProportion", {
  expect_equal(
    tibble(
      customerUID = rep(1:6, 2),
      gender = rep(c("Male", "Female"), each = 6)
    ) %>%
      calcGenderProportion(),
    tibble(
      gender = factor(c("Female", "Male")),
      customers = c(6L, 6L),
      genderProportion = c(0.5, 0.5)
    )
  )
  expect_equal(
    tibble(
      customerUID = rep(1:6, 2),
      gender = rep(c("Male", "Female"), each = 6),
      itemType = rep(c("Fish", "Deer"), 6)
    ) %>%
      calcGenderProportion("itemType"),
    tibble(
      gender = factor(rep(c("Female", "Male"), each = 2)),
      itemType = factor(rep(c("Deer", "Fish"), 2)),
      customers = rep(3L, 4),
      genderProportion = rep(0.5, 4)
    )
  )
})

test_that("calcGenderProportion messages", {
  expect_error(
    calcGenderProportion(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    calcGenderProportion(data.frame(x = 1)),
    "Required variables not present in data frame: customerUID, gender"
  )
  expect_error(
    calcGenderProportion(data.frame(customerUID = 1)),
    "Required variables not present in data frame: gender"
  )
  expect_error(
    calcGenderProportion(data.frame(customerUID = 1), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    calcGenderProportion(data.frame(customerUID = 1, gender = "Male"), "age"),
    "Grouping variables not present in data frame: age"
  )
  expect_error(
    calcGenderProportion(data.frame(customerUID = 1, gender = "Male"), c("age", "itemType")),
    "Grouping variables not present in data frame: age, itemType"
  )
})


# sumRevenue --------------------------------------------------------------

test_that("sumRevenue", {
  expect_equal(
    tibble(
      customerUID = 1:10,
      price = rep(1, 10),
    ) %>%
      sumRevenue(),
    tibble(
      revenue = 10
    )
  )
  expect_equal(
    tibble(
      customerUID = rep(1:10, 2),
      gender = rep(c("Male", "Female"), each = 10),
      price = rep(1, 10 * 2),
    ) %>%
      sumRevenue("gender"),
    tibble(
      gender = factor(c("Male", "Female")),
      revenue = rep(10, 2)
    )
  )
})

test_that("sumRevenue messages", {
  expect_error(
    sumRevenue(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    sumRevenue(data.frame(x = 1)),
    "Required variables not present in data frame: price"
  )
  expect_error(
    sumRevenue(data.frame(customerUID = 1, price = 0), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    sumRevenue(data.frame(customerUID = 1, price = 0), "gender"),
    "Grouping variables not present in data frame: gender"
  )
})

# countItems ------------------------------------------------------------

test_that("countItems", {
  expect_equal(
    tibble(
      customerUID = rep(1:5, 2)
    ) %>%
      countItems(),
    tibble(
      items = 10L
    )
  )
  expect_equal(
    tibble(
      customerUID = rep(1:5, 4),
      gender = rep(c("Male", "Female"), each = 10)
    ) %>%
      countItems("gender"),
    tibble(
      gender = factor(c("Male", "Female")),
      items = c(10L, 10L)
    )
  )
})

test_that("countItems messages", {
  expect_error(
    countItems(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    countItems(data.frame(customerUID = 1), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    countItems(data.frame(customerUID = 1), "gender"),
    "Grouping variables not present in data frame: gender"
  )
  expect_error(
    countItems(data.frame(customerUID = 1), c("age", "gender")),
    "Grouping variables not present in data frame: age, gender"
  )
  expect_error(
    countItems(data.frame(customerUID = 1, gender = "Male"), c("age", "gender")),
    "Grouping variables not present in data frame: age"
  )
})

# calcChurn -------------------------------------------------------


test_that("calcChurn", {
  expect_equal(
    tibble(
      customerUID = 1:6,
      ch = rep("11", 6)
    ) %>%
      separate(col = ch, into = as.character(2010:2011), sep = 1:2) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcChurn(deathThreshold = 1),
    tibble(
      itemYear = factor(2010),
      churnedCustomers = 0,
      retainedCustomers = 6,
      totalCustomers = 6,
      churnRate = 0,
      retentionRate = 1
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:6,
      ch = c(
        "100000", "110000", "101000",
        "100100", "100010", "100001"
      )
    ) %>%
      separate(col = ch, into = as.character(2010:2015), sep = 1:5) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcChurn(deathThreshold = 5),
    tibble(
      itemYear = factor(2010),
      churnedCustomers = 1,
      retainedCustomers = 5,
      totalCustomers = 6,
      churnRate = 1 / 6,
      retentionRate = 5 / 6
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:9,
      ch = c(
        "10000100", "10010000", "10000010",
        "01010000", "01000100", "01000001",
        "00100010", "00100010", "00100000"
      )
    ) %>%
      separate(col = ch, into = as.character(2010:2017), sep = 1:7) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcChurn(deathThreshold = 5) %>%
      as.data.frame(),
    tibble(
      itemYear = factor(c(2010:2012)),
      churnedCustomers = rep(1, 3),
      retainedCustomers = rep(2, 3),
      totalCustomers = rep(3, 3),
      churnRate = rep(1 / 3, 3),
      retentionRate = rep(2 / 3, 3)
    ) %>%
      as.data.frame()
  )
  expect_equal(
    tibble(
      customerUID = 1:9,
      ch = c(
        "10000100", "10010000", "10000010",
        "01010000", "01000100", "01000001",
        "00100010", "00100010", "00100000"
      ),
      gender = factor(rep(c("Male", "Female", "Male"), 3))
    ) %>%
      separate(col = ch, into = as.character(2010:2017), sep = 1:7) %>%
      gather(itemYear, purchase, -customerUID, -gender) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcChurn("gender", deathThreshold = 5) %>%
      as.data.frame(),
    tibble(
      itemYear = factor(rep(c(2010:2012), each = 2)),
      gender = factor(rep(c("Female", "Male"), 3)),
      churnedCustomers = rep(c(0, 1), 3),
      retainedCustomers = rep(c(1, 1), 3),
      totalCustomers = rep(c(1, 2), 3),
      churnRate = rep(c(0, 1 / 2), 3),
      retentionRate = rep(c(1, 1 / 2), 3)
    ) %>%
      as.data.frame()
  )
})

test_that("calcChurn messages", {
  expect_error(
    calcChurn(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    calcChurn(data.frame(x = 1)),
    "Required variables not present in data frame: customerUID, itemYear"
  )
  expect_error(
    calcChurn(data.frame(customerUID = 1, itemYear = 1), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    calcChurn(data.frame(customerUID = 1, itemYear = 1), "gender"),
    "Grouping variables not present in data frame: gender"
  )
  expect_error(
    calcChurn(data.frame(
      customerUID = 1:4,
      itemYear = 2010:2013
    )),
    "Window size is 5, but range of data is only 4 years"
  )
})

# calcRecruitment -------------------------------------------------

test_that("calcAdvRecruitment", {
  expect_equal(
    tibble(
      customerUID = 1:6,
      ch = rep("11", 6)
    ) %>%
      separate(col = ch, into = as.character(2010:2011), sep = 1:2) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcRecruitment(birthThreshold = 1),
    tibble(
      itemYear = factor(2011),
      recruitedCustomers = 0,
      retainedCustomers = 6,
      totalCustomers = 6,
      recruitmentRate = 0
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:6,
      ch = c(
        "11111111", "11111111", "11111111",
        "00000100", "00000010", "00000001"
      )
    ) %>%
      separate(col = ch, into = as.character(2010:2017), sep = 1:7) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcRecruitment(birthThreshold = 5),
    tibble(
      itemYear = factor(c(2015:2017)),
      recruitedCustomers = rep(1, 3),
      retainedCustomers = rep(3, 3),
      totalCustomers = rep(4, 3),
      recruitmentRate = c(1 / 3, 1 / 4, 1 / 4)
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:6,
      ch = c(
        "10001111", "11101111", "01111111",
        "00000100", "10000010", "01000001"
      )
    ) %>%
      separate(col = ch, into = as.character(2010:2017), sep = 1:7) %>%
      gather(itemYear, purchase, -customerUID) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcRecruitment(birthThreshold = 5),
    tibble(
      itemYear = factor(c(2015:2017)),
      recruitedCustomers = rep(1, 3),
      retainedCustomers = rep(3, 3),
      totalCustomers = rep(4, 3),
      recruitmentRate = c(1 / 3, 1 / 4, 1 / 4)
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:9,
      ch = c(
        "11111111", "11111111", "11111111",
        "00000100", "00000100", "00000100",
        "00000001", "00000001", "00000001"
      ),
      gender = factor(rep(c("Male", "Female", "Male"), 3))
    ) %>%
      separate(col = ch, into = as.character(2010:2017), sep = 1:7) %>%
      gather(itemYear, purchase, -customerUID, -gender) %>%
      filter(purchase == 1) %>%
      mutate(itemYear = as.integer(itemYear)) %>%
      calcRecruitment("gender", birthThreshold = 5) %>%
      as.data.frame(),
    tibble(
      itemYear = factor(rep(c(2015:2017), 2)),
      gender = factor(rep(c("Female", "Male"), each = 3)),
      recruitedCustomers = c(1, 0, 1, 2, 0, 2),
      retainedCustomers = rep(c(1, 2), each = 3),
      totalCustomers = c(2, 1, 2, 4, 2, 4),
      recruitmentRate = c(1, 0, 1, 1, 0, 1)
    ) %>%
      as.data.frame()
  )
})

test_that("calcRecruitment messages", {
  expect_error(
    calcRecruitment(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    calcRecruitment(data.frame(x = 1)),
    "Required variables not present in data frame: customerUID, itemYear"
  )
  expect_error(
    calcRecruitment(data.frame(customerUID = 1, itemYear = 1), groupVars = 1),
    "Input 'groupVars' should be a character vector"
  )
  expect_error(
    calcRecruitment(data.frame(customerUID = 1, itemYear = 1), "gender"),
    "Grouping variables not present in data frame: gender"
  )
  expect_error(
    calcRecruitment(data.frame(
      customerUID = 1:4,
      itemYear = 2010:2013
    )),
    "Window size is 5, but range of data is only 4 years"
  )
})


# calcParticipation --------------------------------------------------
test_that("calcParticipation", {
  expect_equal(
    tibble(
      customerUID = 1:10,
      county = rep(c("Lancaster", "Douglas"), each = 5)
    ) %>%
      calcParticipation("county",
        pop = tibble(
          year = 2016,
          gender = rep(c("Male", "Female"), 2),
          county = rep(c("Lancaster", "Douglas"), each = 2),
          population = c(10, 20, 10, 20)
        )
      ),
    tibble(
      county = c("Douglas", "Lancaster"),
      customers = c(5L, 5L),
      pop = c(30, 30),
      participationRate = c(5 / 30, 5 / 30)
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:12,
      gender = rep(c("Male", "Female"), 6),
      county = rep(c("Lancaster", "Douglas"), each = 6)
    ) %>%
      calcParticipation(c("county", "gender"),
        pop = tibble(
          year = 2016,
          gender = rep(c("Male", "Female"), 2),
          county = rep(c("Lancaster", "Douglas"), each = 2),
          population = c(10, 20, 10, 20)
        )
      ),
    tibble(
      county = rep(c("Douglas", "Lancaster"), each = 2),
      gender = factor(rep(c("Female", "Male"), 2)),
      customers = c(3L, 3L, 3L, 3L),
      pop = c(20, 10, 20, 10),
      participationRate = c(3 / 20, 3 / 10, 3 / 20, 3 / 10)
    )
  )
  expect_equal(
    tibble(
      customerUID = 1:12,
      gender = rep(c("Male", "Female"), 6),
      county = factor(rep(c("Lancaster", "Douglas"), each = 6))
    ) %>%
      calcParticipation(c("county", "gender"),
        pop = tibble(
          year = 2016,
          gender = rep(c("Male", "Female"), 2),
          county = rep(c("Lancaster", "Douglas"), each = 2),
          population = c(10, 20, 10, 20)
        )
      ) %>%
      colnames(),
    c("county", "gender", "customers", "pop", "participationRate")
  )
})


# itemGroupCount --------------------------------------------------------

widePermitData <-
  tibble(
    customerUID = 1:6,
    Fish = c(1, 1, 1, 1, 1, 1),
    Deer = c(1, 0, 1, 0, 0, 1),
    Hunt = c(0, 0, 1, 0, 0, 0)
  )

permitData <-
  widePermitData %>%
  gather(itemType, purchase, -customerUID) %>%
  filter(purchase == 1) %>%
  select(-purchase)

comboCount <-
  widePermitData %>%
  group_by(Fish, Deer, Hunt) %>%
  summarize(customers = n_distinct(customerUID)) %>%
  ungroup() %>%
  arrange(desc(customers)) %>%
  mutate(groupID = row_number()) %>%
  gather(itemType, purchase, -customers, -groupID) %>%
  arrange(groupID) %>%
  group_by(groupID) %>%
  mutate(degree = sum(purchase)) %>%
  ungroup()

test_that("itemGroupCount", {
  expect_equal(
    permitData %>%
      itemGroupCount(),
    comboCount
  )
})

test_that("itemGroupCount messages", {
  expect_error(
    itemGroupCount(1),
    "Input 'df' should be a data frame"
  )
  expect_error(
    itemGroupCount(data.frame(x = 1)),
    "Required variables not present in data frame: customerUID, itemType"
  )
})
