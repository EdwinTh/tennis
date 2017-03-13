library(dplyr)

three_matches <- atp_matches %>%
  filter(winner_name == "Roger Federer") %>% slice(1:3)

six_matches <- rbind(three_matches, atp_matches %>%
                      filter(loser_name == "Roger Federer") %>% slice(1:3) )

atp_matches %>% filter(winner_name %in% c("Roger Federer", "Rafael Nadal"),
                       loser_name %in% c("Roger Federer", "Rafael Nadal"),
                       substr(tourney_date, 1, 4) == "2007")

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


context("find_matchups helper functions")

test_that("check_players_a_b helper function", {
  expect_error(check_players_a_b("Roger Federer", NULL))
  expect_error(check_players_a_b(NULL, "Roger Federer"))
  expect_error(check_players_a_b("Roger Federer", "Roger Federer"))
})

test_that("find_matchup helper function", {
  expect_equal(get_matchup(six_matches, 'Roger Federer', "Jan Siemerink") %>% nrow, 1)
  expect_equal(get_matchup(six_matches, 'Roger Federer',
                           c("Jan Siemerink", "Guillaume Raoux")) %>% nrow, 2)
  expect_equal(get_matchup(atp_matches %>% filter(substr(tourney_date, 1, 4) == "2007"),
                           c("Roger Federer", "Rafael Nadal"),
                           c("Roger Federer", "Rafael Nadal")) %>% nrow, 5)

})

context("tennis_reshape helper functions")

test_that("make_player_perspective helper function", {
  expect_equal(
    make_player_perspective(six_matches, "Roger Federer", "won") %>% nrow, 3)
  expect_equal(
    make_player_perspective(six_matches, "Roger Federer", "lost") %>% nrow, 3)
  expect_equal(
    make_player_perspective(six_matches, c("Roger Federer", "Jan Siemerink"), "won") %>% nrow, 4)
})

test_that("adjust_columns_won_lost helper function", {
  pp <- rbind(make_player_perspective(six_matches, "Roger Federer", "won"),
              make_player_perspective(six_matches, "Roger Federer", "lost"))
  expect_equal(adjust_columns_won_lost(pp) %>% slice(1:3) %>%
                 select(player_ht) %>% unlist %>% unname, rep(185, 3))
  expect_equal(adjust_columns_won_lost(pp) %>% slice(4:6) %>%
                 select(player_ht) %>% unlist %>% unname, rep(185, 3))
})
