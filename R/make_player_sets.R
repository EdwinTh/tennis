#' Create a data frame with list columns of
#'   every combination of active players that
#'   played at least three matches against each other.
#' @param x A data frame with all the matches in scope
#' @return A data frame with three columns:
#'   player1 - player2 - matches. Last is a list column with all
#'   the matches played. Player 1 is always the first alphabetically.
make_player_sets <- function(x = readRDS("data_processed/atp.Rds")) {
  all_active_players <- find_all_active_players(x)
  combs_with_three_or_more <- combs_with_x_matches(all_active_players, 3)

}


find_all_active_players <- function(x,
                                    active_year = 2019) {
  x %>%
    filter_active_year(active_year) %>%
    select(winner_name, loser_name) %>%
    pivot_longer(everything(), values_to = "player") %>%
    distinct(player)
}


filter_active_year <- function(x, yr) {
  has_cols(x, "tourney_date")
  ret <- x %>%
    filter(substr(tourney_date, 1, 4) == yr)
  if (nrow(ret) == 0) {
    stop(yr, " is not a year in tourney_date")
  }
  ret
}
