library(dplyr)

three_matches <- atp_matches %>%
  filter(winner_name == "Roger Federer") %>% slice(1:3)

six_matches <- rbind(three_matches, atp_matches %>%
                      filter(loser_name == "Roger Federer") %>% slice(1:3) )

context("find_players helper function")
test_that("check_x filters out wrong inputs",{
  expect_silent(check_x(three_matches, "both"))
  expect_silent(check_x(three_matches, "won"))
  expect_silent(check_x(three_matches, "lost"))
  expect_error(check_x(three_matches$winner_name,  "both"))
  expect_error(check_x(three_matches %>% select(-winner_name),  "won"))
  expect_error(check_x(three_matches %>% select(-winner_name),  "both"))
  expect_error(check_x(three_matches %>% select(-loser_name),  "lost"))
})

test_that("get_ddd helper function", {
  produce_fun_args <- function(x, ...)  as.list(match.call())
  fun_args <-
    produce_fun_args(mtcars, name1 =  "Edwin", "Thoen", won_lost = "won")
  expect_error( get_ddd(produce_fun_args(x)) )
  expect_length( get_ddd(fun_args), 2 )
  expect_length( get_ddd(fun_args, exclude = "name1"), 1 )
})

test_that("get_matches helper function", {
  expect_equal(get_matches(six_matches, 'Roger Federer', "both") %>% nrow, 6)
  expect_equal(get_matches(six_matches, 'roger federer', "both") %>% nrow, 6)
  expect_equal(get_matches(six_matches, 'Roger Federer', "won")$winner_name,
               rep("Roger Federer", 3))
  expect_equal(get_matches(six_matches, 'Roger Federer', "lost")$loser_name,
               rep("Roger Federer", 3))
  expect_equal(get_matches(six_matches,
                           players = c('Roger Federer', "Jan Siemerink"), "won") %>% nrow, 4)
})

test_that("which_missing helper function", {
  expect_silent(which_missing(three_matches, "Roger Federer"))
  expect_silent(which_missing(three_matches, "roger federer"))
  expect_equal(which_missing(three_matches, "roge federer"), "roge federer")
  expect_equal(which_missing(three_matches, c("Roger Federer", "Rafael Nadal")),
               "Rafael Nadal")
})

test_that("get_closest_levenstein function", {
  a <- "Roger Federe"; b <- c("Roger Federer", "Rafael Nadal")
  expect_warning(get_closest_levenstein(a, b),
                 "Could not find Roger Federe, did you mean Roger Federer?")
})
