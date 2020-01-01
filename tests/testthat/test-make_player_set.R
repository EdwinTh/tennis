context("create players set")

x <- tibble(tourney_date = c("20180101", "20190701"))

test_that("filter_active_year gives correct output", {
  expect_equal(filter_active_year(x, 2018), x[1, ])
  expect_equal(filter_active_year(x, 2019), x[2, ])
})

test_that("filter_active_year throws error when year is not existent",{
  expect_error(filter_active_year(x, 2017),
               "2017 is not a year in tourney_date")
})
