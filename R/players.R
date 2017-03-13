#' Find all the matches of a player
#'
#' Query function to return the matches of one or more players in the data set.
#' Function is case-insensitive, all names will be set to lower before matching.
#' @param x A data frame from the \code{tennis} package.
#' @param ... Character string of the full player name(s).
#' Multiple player names are separated by a comma.
#' @param won_lost Character string indicating if only the won matches,
#' lost matches, or both should be returned.
#' @return A subset of \code{x} with the matches of the requested players.
#' @examples
#' library(dplyr)
#' atp_matches %>% find_player("roger federer", "Rafael Nadal", "NOVAK DJOKOVIC")
find_player <- function(x,..., won_lost = c('both', 'won', 'lost')) {
  won_lost <- match.arg(won_lost)
  check_x(x, won_lost)
  players  <- get_ddd(as.list(match.call()))
  full_set <- get_matches(x, players, won_lost)
  check_players_found(x, full_set, players)
  return(full_set)
}

# make sure the correct input comes in.
check_x <- function(x, won_lost) {
  stopifnot(is.data.frame(x))

  if(won_lost != "lost") {
    stopifnot("winner_name" %in% colnames(x))
  } else if (won_lost != "won") {
    stopifnot("loser_name" %in% colnames(x))
  }
}

# distill the relevant arguments from the input arguments
get_ddd <- function(fun_args, exclude = NULL) {
  if (length(fun_args) < 3) {
    stop("No names provided to find in x", call. = FALSE)
  }
  exclude <- c(exclude, c("x", "won_lost"))
  ddd <- fun_args[3:length(fun_args)]
  ddd_only <- ddd[!names(ddd) %in% exclude]
  as.character(ddd_only)
}

# get all the matches of a player
get_matches <- function(x, players, won_lost) {
  full_set <- data.frame()
  if (won_lost != "lost") {
    full_set  <- dplyr::filter(x, tolower(winner_name) %in% tolower(players))
  }
  if (won_lost != "won") {
    lost <- dplyr::filter(x, tolower(loser_name) %in% tolower(players))
    full_set <- rbind(full_set, lost)
  }
  return(full_set)
}

# helper function that checks if there are players not found
which_missing <- function(x, pl) {
  players_found <- tolower( c(x$winner_name, x$loser_name))
  pl[!tolower(pl) %in% players_found]
}

# if there are missing players, suggest the player with the closest
# levenstein.
get_closest_levenstein <- function(set_a, set_b) {
  dist_mat <- stringdist::stringdistmatrix(set_a, set_b, method = "lv")
  closest <- set_b[apply(dist_mat, 1, which.min)]

  for(i in 1:length(closest)) {
    message  <- sprintf("Could not find %s, did you mean %s?", set_a[i], closest[i])
    warning(message, call. = FALSE)
  }
}

#' Find a matchup between two or more players
#'
#' Find the matches between two or more specific opponents. If only
#' \code{players_a} is specified all the matches between the players in this
#' set are returned. If both \code{players_a} and \code{players_b} are
#' specified all the matched betweena player in set a and a player in set b
#' are specified. Matching is case-insensitive.
#' @param x A data frame from the \code{tennis} package.
#' @param players_a A character vector.
#' @param players_a A character vector.
#' @return A subset of \code{x} with the matchups between the requested players.
#' @examples
#' # Find all matches between Nadal and Federer:
#' find_matchup(atp_matches, "Roger Federer", "Rafael Nadal")
#'
#' # Find all matches between Nadal, Djokovic, and Federer:
#' find_matchup(atp_matches, c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))
#'
#' Find all matches that Nadal or Federer played against Djokovic or Murray:
#' a <- c("Roger Federer", "Rafael Nadal"); b <- c("Novak Djokovic", "Andy Murray")
#' find_matchup(atp_matches, a, b)
#'
#' Find all matches that Nadal and Federe played against Djokovic or Murray
#' and all the matches they played against each other:
#' find_matchup(atp_matches, a, c(a, b))

find_matchup <- function(x, players_a, players_b = players_a) {
  check_x(x, "both")
  check_players_a_b(players_a, players_b)
  full_set <- get_matchup(x, players_a, players_b)
  check_players_found(x, full_set, unique(c(players_a, players_b)))
  return(full_set)
}

check_players_a_b <- function(a, b) {
  stopifnot(length(a) > 0)
  stopifnot(length(b) > 0)
  if ( length(unique(c(a, b))) == 1 ) {
    stop("Only one player provided, can't make matchup", call. = FALSE)
  }
}

get_matchup <- function(x, a, b) {
  a <- tolower(a); b <- tolower(b)
  full_set <- dplyr::filter(x,
                            (tolower(winner_name) %in% a &
                               tolower(loser_name) %in% b) |
                              (tolower(winner_name) %in% b &
                                 tolower(loser_name) %in% a))
}

#' Reshape the tennis data from the perspective of one player
#'
#' Each match will be converted  so its from the perspective of one player. When
#' both players of a match are queried, two records will be created from one
#' match. The reshaped data makes it easier to analyse the results of a single
#' player. Function is case-insensitive.
#' @param x A data frame from the \code{tennis} package.
#' @param ... Character string of the full player name(s).
#' Multiple player names are separated by a comma. If empty,
#' all players in the set will be retained (and the data set will
#' double in rows).
#' @return For each match played by a requested player a record is created. The
#' columns \code{player}, \code{won_lost}, and \code{opponent} replace the
#' columns \code{winner_name} and \code{loser_name}. All the other columns
#' containing player information are renamed to either player_ or opponent_
#' (and p_ or o_) depending to which of the players they belong.
tennis_reshape <- function(x, ...) {
  check_x(x, "both")
  if (length(as.list(match.call()) == 2)) {
    players <- unique(c(x$winner_name, x$loser_name))
  } else {
    players <- get_ddd(as.list(match.call()))
  }

  full_set <- rbind(make_player_perspective(x, players, "won"),
                    make_player_perspective(x, players, "lost"))

  full_set <- adjust_columns_won_lost(full_set)
  full_set <- dplyr::arrange(full_set, player)


  check_players_found(x, full_set, players)

  return(full_set)
}

# reshape the data so it is from the perspective of one data
make_player_perspective <- function(x, players, won_or_lost) {
  filter_set <- data_frame(player = players)
  if (won_or_lost == "won") {
    filter_set$result <- "won"
    winner_set <- inner_join(filter_set, x, by = c("player" = "winner_name"))
    return(select(winner_set, player, result, opponent = loser_name, everything()))
  } else {
    filter_set$result <- "lost"
    winner_set <- inner_join(filter_set, x, by = c("player" = "loser_name"))
    return(select(winner_set, player, result, opponent = winner_name, everything()))
  }
}

# rename the column names after reshaping, winner and loser are replaced by
# player and opponent, so opponent info is equal irrespective of win or lose
adjust_columns_won_lost <- function(x) {
  cols <- colnames(x)
  win_cols <- stringr::str_replace_all(cols, c("winner" = "player", "^w" = "p",
                                   "loser" = "opponent", "^l" = "o"))
  won <- dplyr::filter(x, result == "won")
  colnames(won) <- win_cols

  lose_cols <- stringr::str_replace_all(cols, c("loser" = "player", "^l" = "p",
                                   "winner" = "opponent", "^w" = "o"))
  lost <- dplyr::filter(x, result == "lost")
  colnames(lost) <- lose_cols
  rbind(won, lost)
}

# refactored wrapper over the checking of missing chars
check_players_found <- function(x, full_set, players) {
  players_not_found <- which_missing(full_set, players)
  all_available_players <- unique( c(x$winner_name, x$loser_name))
  if (length(players_not_found) > 0) {
    get_closest_levenstein(players_not_found, all_available_players)
  }
}


