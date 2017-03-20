library(dplyr)

three_matches <- atp_matches %>%
  filter(winner_name == "Roger Federer") %>% slice(1:3)

three_matches_with_dc <-
  rbind(three_matches,
        atp_matches %>%
          filter(tourney_id == "2017-M-DC-2017-WG-M-SUI-USA-01") %>%
          slice(1))

no_id <- three_matches %>% select(-tourney_id)
no_name <- three_matches %>% select(-tourney_name)

context("find_tourney function helpers")

test_that("check_x_tourney helper function", {
  expect_silent(check_x_tourney(three_matches))
  expect_error(check_x_tourney(three_matches$tourney_id),
               "is.data.frame\\(x\\) is not TRUE")
  expect_error(check_x_tourney(three_matches %>%
                                 select(-c(tourney_id, tourney_name))))
  expect_warning(check_x_tourney(three_matches %>%
                                   select(-tourney_id)))
  expect_warning(check_x_tourney(three_matches %>%
                                   select(-tourney_name)))
})

test_that("get_tourney_id helper function", {
  expect_equal(get_tourney_id(three_matches), c("327", "327", "328"))
  expect_equal(get_tourney_id(three_matches_with_dc), c("327", "327", "328", NA))
})

test_that("extract_ helper functions", {
  expect_equal(extract_ids(three_matches), c("327", "327", "328"))
  expect_true(extract_ids(no_id) %>% is.na)
  expect_equal(extract_names(three_matches), c("toulouse", "toulouse", "basel"))
  expect_true(extract_names(no_name) %>% is.na)
})

test_that("filter_tourneys helper function", {
  nm <- c("toulouse", "toulouse", "basel"); nm_na <- NA
  id <- c("327", "327", "328"); id_na <- NA
  tourneys <- c("Toulouse", 328)
  expect_equal( filter_tourneys(three_matches, nm, id, tourneys) %>% nrow, 3)
  expect_equal( filter_tourneys(three_matches, nm, id_na, tourneys) %>% nrow, 2)
  expect_equal( filter_tourneys(three_matches, nm_na, id, tourneys) %>% nrow, 1)
})

test_that("check_tourneys_found helper function" ,{
  expect_silent( check_tourneys_found(three_matches %>% slice(1:2), "Toulouse") )
  expect_silent( check_tourneys_found(three_matches %>% slice(1:2) %>%
                                        select(-tourney_id), "Toulouse") )
  expect_silent( check_tourneys_found(three_matches %>% slice(1:2), "327") )
  expect_silent( check_tourneys_found(three_matches %>% slice(1:2), 327) )
  expect_silent( check_tourneys_found(three_matches %>% slice(1:2) %>%
                                        select(-tourney_name), 327) )
  expect_warning( check_tourneys_found(three_matches %>% slice(1:2), "Jos") )
  expect_warning( check_tourneys_found(three_matches %>% slice(1:2), "111") )
  expect_warning( check_tourneys_found(three_matches %>% slice(1:2), 111) )
})
