library(dplyr)

three_matches <- atp_matches %>%
  filter(winner_name == "Roger Federer") %>% slice(1:3)

context("find_time function helpers")

test_that("check_x_time helper function", {
  expect_silent(check_x_time(three_matches))
  expect_error(check_x_time(three_matches$tourney_date))
  expect_error(check_x_time(three_matches %>% select(-tourney_date) ))
})

test_that("ensure_integer helper function", {
  expect_equal(ensure_integer(2010), 2010)
  expect_equal(ensure_integer("2010"), 2010)
  expect_equal(ensure_integer("2010-01-01"), 20100101)
  expect_equal(ensure_integer( as.Date("2010-01-01")), 20100101)
})

test_that("check_from_to helper function", {
  expect_silent(check_from_to(2010))
  expect_error(check_from_to(NA))
  expect_error(check_from_to(201101011))
})

test_that("round_data helper functions", {
  expect_equal( round_data(2010, "from"), 20100000)
  expect_equal( round_data(201001, "from"), 20100100)
  expect_equal( round_data(20100101, "from"), 20100101)
  expect_equal( round_data(2010, "to"), 20109999)
  expect_equal( round_data(201001, "to"), 20100199)
  expect_equal( round_data(20100101, "to"), 20100101)
})
