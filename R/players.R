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
find_matchup <- function(x, players_a, players_b = players_a) {
  stopifnot(is.data.frame(x))
  stopifnot("winner_name" %in% colnames(x))
  stopifnot("loser_name" %in% colnames(x))

  players_a_low <- tolower(players_a)
  players_b_low <- tolower(players_b)

  full_set <- dplyr::filter(x,
    (tolower(winner_name) %in% players_a_low &
       tolower(loser_name) %in% players_b_low) |
    (tolower(winner_name) %in% players_b_low &
       tolower(loser_name) %in% players_a_low)
  )

  all_players_asked <- unique(tolower(c(players_a, players_b)))

  players_not_found <- which_missing(full_set, all_players_asked)

  all_available_players <- c(unique(x$winner_name), unique(x$loser_name))

  if (length(players_not_found) > 0) {
    get_closest_levenstein(tolower(players_not_found),
                           tolower(all_available_players))}
  return(full_set)
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
