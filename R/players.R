#' Find all the matches of a player
#'
#' Query function to return the matches of one or more players in the data set.
#' Function is case-insensitive, all names will be set to lower before matching.
#' @param x A data frame from the \code{tennis} package.
#' @param ... Character string of the full player name.
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
  players_not_found <- which_missing(full_set, players)
  all_available_players <- c(unique(x$winner_name), unique(x$loser_name))
  if (length(players_not_found) > 0) {
    get_closest_levenstein(players_not_found, all_available_players)
  }
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
#' are specified.
#' @param x A data frame from the \code{tennis} package.
#' @param players_a A character vector.
#' @param players_a A character vector.
#' @return A subset of \code{x} with the matchups between the requested players.
#' @examples
#' # Find all matches between Nadal, Djokovic, and Federer:
#' find_matchup(atp_matches, c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))
#'
#' Find all matches that Nadal or Federer played against Djokovic or Murray:
#' a <- c("Roger Federer", "Rafael Nadal"); b <- c("Novak Djokovic", "Andy Murray")
#' find_matchup(atp_matches, a, b)
#'
#' Find all matches that Nadal and layed against Djokovic or Murray and all the
#' matches they played against each other:
#' find_matchup(atp_matches, a, c(a, b))



find_matchup <- function(x, players_a, players_b = players_a) {
  check_x(x, "both")
  check_players_a_b(players_a, players_b)
  full_set <- get_matchup(x, players_a, players_b)
  players_not_found <- which_missing(full_set, unique(c(players_a, players_b)))
  all_available_players <- c(unique(x$winner_name), unique(x$loser_name))
  if (length(players_not_found) > 0) {
    get_closest_levenstein(players_not_found, all_available_players)
  }
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
#' @param x A data frame from the \code{tennis} package.
#' @param ... Character string of the full player name.
#' Multiple player names are separated by a comma. If empty,
#' all players in the set will be retained (and the data set will
#' double in size).
player_won_lost <- function(x, ...) {
  stopifnot(is.data.frame(x))
  stopifnot("winner_name" %in% colnames(x))
  stopifnot("loser_name" %in% colnames(x))

}


players_a <- c('roger federer', 'rafael nadal',  'geert wilders')
players_b <- c('novak djokovic', 'robin haase')
atp_matches %>% find_matchup(players_a, players_b)
