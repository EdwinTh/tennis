
find_tourney <- function(x, ... ) {
  stopifnot(is.data.frame(x))
  stopifnot("tourney_id" %in% colnames(x))

  fun_args <- as.list(match.call())
  tourneys <- tolower(as.character(fun_args[3:length(fun_args)]))

  # remove davis cup matches, because they don't have an ID
  if ("tourney_level" %in% colnames(x)) {
    x <- dplyr::filter(x, tourney_level != 'D')
  }

  tourney_ids <- tolower(get_tourney_id(x))
  tourney_names <- tolower(x$tourney_name)

  ind <- (tourney_names %in% tourneys) |
    (tourney_ids %in% tourneys)
  full_set <- x[ind, ]

  found <- unique(c(tourney_names[ind], tourney_ids[ind]))
  missing_names <- tourneys[! tourneys %in% found]

  if (length(missing_names) > 0) {
    for(n in missing_names) {
      warning(sprintf("Could not find %s", n), call. = FALSE)
    }
  }

  return(full_set)

}
find_tourney(atp_matches, "580", "wimbledon", "roland garros", "Jos")

get_tourney_id <- function(df) {
  splitted <- strsplit(df$tourney_id, '-')
  sapply(splitted, function(x) x[2])
}

# function that will distill from draw_size and the match_num
# what the round of the match was within the tournament.
