context("find_all_active_players")

x <- tibble(tourney_date = c("20180101", "20190701"),
            winner_name  = c("Adriaantje", "B100"),
            loser_name   = c("Bassie", "Rooie Japie"))

test_that("find_all_active_players correct results", {
  expect_equal(find_all_active_players(x),
               c("B100", "Rooie Japie"))
  expect_equal(find_all_active_players(x, 2018),
               c("Adriaantje", "Bassie"))
})

context("get_all_matches")
test_that("get_all_matches has correct output, one player", {
  expect_equal(get_all_matches(x, "Bassie"), x[1, ])
  expect_equal(get_all_matches(x, "B100"), x[2, ])
  expect_equal(get_all_matches(x, c("Bassie", "B100")), x)
})

test_that("get_all_matches has correct output, both players", {
  expect_error(get_all_matches(x, "Bassie", "2"),
               "No matches in the selection")
  expect_error(get_all_matches(x, c("Bassie", "B100"), "2"),
               "No matches in the selection")
  expect_equal(get_all_matches(x, c("Bassie", "Adriaantje"), "2"), x[1, ])
  expect_equal(get_all_matches(x, c("Bassie", "Adriaantje", "B100"), "2"), x[1, ])
  expect_equal(get_all_matches(x, c("Bassie", "Adriaantje", "B100", "Rooie Japie"), "2"), x)
})

context("filter_active_year")

test_that("filter_active_year gives correct output", {
  expect_equal(filter_active_year(x, 2018), x[1, ])
  expect_equal(filter_active_year(x, 2019), x[2, ])
})

test_that("filter_active_year throws error when year is not existent",{
  expect_error(filter_active_year(x, 2017),
               "2017 is not a year in tourney_date")
})

context("making matchup")

x_match_up <- bind_rows(
  x,
  tibble(tourney_date = "20190801", winner_name = "Bassie", loser_name = "Adriaantje")
)

test_that("add_matchup gives correct ouput", {
  output <- x_match_up
  output$player1 <- c("Adriaantje", "B100", "Adriaantje")
  output$player2 <- c("Bassie", "Rooie Japie", "Bassie")
  expect_equal(add_matchup(x_match_up), output)
})

test_that("count_matchup gives correct ouptut", {
  expect_identical(
    count_matchup(x_match_up[1, ]),
    tibble(player1 = "Adriaantje", player2 = "Bassie", n = 1L)
  )

  expect_identical(
    count_matchup(x_match_up[3, ]),
    tibble(player1 = "Adriaantje", player2 = "Bassie", n = 1L)
  )

  expect_identical(
    count_matchup(x_match_up),
    tibble(player1 = c("Adriaantje", "B100"), player2 = c("Bassie", "Rooie Japie"), n = c(2L, 1L))
  )
})

test_that("matchups_with_x_matches gives correct output", {
  expect_equal(matchups_with_x_matches(x_match_up, min_nr = 1), x_match_up)
  expect_equal(matchups_with_x_matches(x_match_up, min_nr = 2), x_match_up[-2, ])
})
