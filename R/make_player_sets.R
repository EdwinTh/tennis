#' Create a data frame with list columns of
#'   every combination of active players that
#'   played at least three matches against each other.
#' @param full_set A data frame with all the matches in scope
#' @return A data frame with three columns:
#'   player1 - player2 - matches. Last is a list column with all
#'   the matches played. Player 1 is always the first alphabetically.
make_player_sets <- function(full_set = readRDS("data_processed/atp.Rds")) {
  full_set_cl              <- clean_full_set(full_set)
  all_active_players       <- find_all_active_players(full_set)
  all_active_players_df    <- get_all_matches(full_set, all_active_players, nr_active = "2")
  combs_with_three_or_more <- matchups_with_x_matches(all_active_players_df)
  combs_with_three_or_more %>%
    add_matchup() %>%
    nest(-c(player1, player2))
}

clean_full_set <- function(x) {
  x %>%
    filter(surface %in% c("Carpet", "Clay", "Grass", "Hard"))
}

find_all_active_players <- function(x,
                                    active_year = 2019) {
  x %>%
    filter_active_year(active_year) %>%
    select(winner_name, loser_name) %>%
    pivot_longer(everything(), values_to = "player") %>%
    distinct(player) %>%
    pull()
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

# both the winner and the loser should be active
get_all_matches <- function(x, all_active_players, nr_active = c("1", "2")) {
  has_cols(x, "winner_name", "loser_name")
  nr_active <- match.arg(nr_active)
  if (nr_active == "1") {
    ret <- bind_rows(
      x %>% filter(winner_name %in% all_active_players),
      x %>% filter(loser_name %in% all_active_players)
    )
  } else {
    ret <- x %>%
      filter(winner_name %in% all_active_players &
               loser_name %in% all_active_players)
  }
  if (nrow(ret) == 0) {
    stop("No matches in the selection")
  }
  ret
}

matchups_with_x_matches <- function(x, min_nr = 3) {
  count_selection <- x %>%
    count_matchup() %>%
    filter(n >= min_nr)
  inner_join(add_matchup(x),
             count_selection,
             by = c("player1", "player2")) %>%
    select(-player1, -player2, -n)
}

count_matchup <- function(x) {
  x %>%
    add_matchup() %>%
    count(player1, player2)
}

add_matchup <- function(x) {
  has_cols(x, "winner_name", "loser_name")
  x %>%
    mutate(player1 = pmin(winner_name, loser_name),
           player2 = pmax(winner_name, loser_name))
}
