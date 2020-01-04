
create_surface_function <- function(surface_vec) {
  stopifnot(is.character(surface_vec))
  ref_cat <- surface_vec[1]
  non_refs <- setdiff(ref_cat)
}

features_one_matchup <- function(p1, matchup) {
  matchup$surface
}

create_base_features <- function(matchups) {
  matchup
}
