#' Filter Tennis Data on Time
#'
#' Return all matches in \code{x} that were played between two dates.
#' Matches played on \code{to} or \code{from} are included in te returned data.
#' @param x A data frame from the \code{tennis} package.
#' @param from A vector of length one that indicates the starting point.
#' See Details.
#' @param to A vector of length one that indicates the end point.
#' See Details.
#' @return The subset in \code{x} of all the matches that were played
#' between \code{from} and \code{to}.
#' @details The values for \code{from} and \code{to} are compared to the
#' integer field \code{tourney_date} in \code{x}. When they are nonnumeric,
#' for instance of class \code{Date}, they are cleaned and coerced to integer.
#' \code{from} and \code{to} may contain less than eight characters, they will
#' padded if necesarry.
#' @examples
#' # get all matches in 2016
#' filter_time(atp_matches, 2016)
#'
#' # get all matches from tourneys that start in June 2016
#' filter_time(atp_matches, 201606)
#'
#' # get all matches from tourneys started between
#' # June 2015 and May 2016
#' filter_time(atp_matches, "2015-06", "2016-05")
#' @export
filter_time <- function(x, from, to = from) {
  check_x_time(x)
  from <- create_from_to(from, "from")
  to   <- create_from_to(to, "to")
  dplyr::filter(x, tourney_date >= from, tourney_date <= to)
}

check_x_time <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot("tourney_date" %in% colnames(x))
}

ensure_integer <- function(x) {
  if(!is.numeric(x)) {
    numerics <-
      paste(unlist(
        stringr::str_extract_all(as.character(x), '[0-9]')),
        collapse = '')
    as.integer(numerics)
  } else {
    ret <- x
  }
}

check_from_to <- function(x) {
  if (is.na(x) | nchar(x) > 8) {
    stop("Invalid value for from or to", call. = FALSE)
  }
}

round_data <- function(x, direction = c("from", "to")) {
  direction <- match.arg(direction)
  if (direction == "from") {
    as.integer( substr( paste(x, "0000000", sep = ''), 1, 8) )
  } else {
    as.integer( substr( paste(x, "9999999", sep = ''), 1, 8) )
  }
}

create_from_to <- function(x, which = c("from", "to")) {
  which <- match.arg(which)
  if (!is.na(x)) {
    x <- ensure_integer(x)
    x <- check_from_to(x)
    ret <- round_data(x, which)
  } else {
    ret <- ifelse(which == "from", 0, 999999999)
  }
  return(ret)
}

