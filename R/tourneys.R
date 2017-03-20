#' Filter Tennis Data on Tournament
#'
#' Select one or more tournaments from \code{x}.
#' @param x A data frame from the \code{tennis} package.
#' @param ... Either the name or the id of the tournament(s). Might
#' be used together. The ids are formed by the last three characters
#' of \code{tourney_id}.
#' @param x A data frame from the \code{tennis} package.
#' @param ... A vector with the ids and/or tournaments.
#' @return A subset of \code{x} with the matches of the requested tournaments.
#' @examples
#' # find all the Grand Slam matches in 2016
#' library(dplyr)
#' atp_matches %>% filter_time(2016) %>%
#'     filter_tourney("Australian Open", "Roland Garros", "Wimbledon", "US Open")
#'
#' find all the Grand Slam matches from 2010 to 2015
#' atp_matches %>% filter_time(2010, 2015) %>%
#'    filter_tourney(520, 540, 560, 580)
#'
#' find all matches at Sydney, Brisbane and Australian Open
#' atp_matches %>% filter_tourney("Sydney", "Brisbane", 580)
#' @export
filter_tourney <- function(x, ... ) {
  check_x_tourney(x)
  tourneys <- get_ddd(as.list(match.call()))

  tourney_names <- extract_name(x)
  tourney_ids   <- extract_ids(x)

  full_set <- filter_tourneys(x, tourney_names, tourney_ids, tourneys)
  check_tourneys_found(full_set, tourneys)
  return(full_set)
}

check_x_tourney <- function(x){
  stopifnot(is.data.frame(x))
  name_missing <- !"tourney_name" %in% colnames(x)
  id_missing   <- !"tourney_id" %in% colnames(x)

  if (name_missing & !id_missing) {
    warning("tourney_name is missing in x, can only use tourney_id")
  } else if (!name_missing & id_missing) {
    warning("tourney_id is missing in x, can only use tourney_name")
  } else if (name_missing & id_missing) {
    stop("Both tourney_id and tourney_name are missing in x", call. = FALSE)
  }
}

get_tourney_id <- function(x) {
  splitted <- strsplit(x$tourney_id, '-')
  ids <- sapply(splitted, function(x) x[2])
  ids_as_num <- suppressWarnings(as.numeric(ids))
  ids[is.na(ids_as_num)] <- NA
  ids
}

extract_ids <- function(x) {
  if ("tourney_id" %in% colnames(x)) {
    return(tolower(get_tourney_id(x)))
  } else {
    return(NA)
  }
}

extract_names <- function(x) {
  if ("tourney_name" %in% colnames(x)) {
    return(tolower(x$tourney_name))
  } else {
    return(NA)
  }
}

filter_tourneys <- function(x, nm, id, tourneys) {
  nm_found <- nm %in% tolower(tourneys)
  id_found <- id %in% tourneys
  ind <- nm_found | id_found
  return(x[ind, ])
}

check_tourneys_found <- function(full_set, tourneys){
  if ("tourney_id" %in% colnames(full_set) ) {
    ids <- get_tourney_id(full_set)
  } else {
    ids <- NA
  }
  if ("tourney_name" %in% colnames(full_set) ) {
    names <- tourney_names
  } else {
    names <- NA
  }
  found <- unique(c(tolower(names), ids))
  missing_names <- tourneys[! tolower(tourneys) %in% found]

  if (length(missing_names) > 0) {
    for(n in missing_names) {
      warning(sprintf("Could not find %s", n), call. = FALSE)
    }
  }
}
