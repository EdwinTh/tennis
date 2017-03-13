
filter_time <- function(x, from, to = from) {
  stopifnot(is.data.frame(x))
  stopifnot("tourney_date" %in% colnames(x))
  from <- check_integer(from); to <- check_integer(to)
  from <- as.integer( substr( paste(from, "0000000", sep = ''), 1, 8) )
  to <- as.integer( substr( paste(to, "9999999", sep = ''), 1, 8) )
  dplyr::filter(x,tourney_date >= from, tourney_date <= to)
}

check_integer <- function(x) {
  if(!is.numeric(x)) {
    numerics <-
      paste(unlist(
        stringr::str_extract_all(as.character(x), '[0-9]')),
        collapse = '')
    as.integer(numerics)
  } else {
    x
  }
}


atp_matches_qual_chal %>% filter_time(from = 2010, to = as.Date("2010-06-01"))
