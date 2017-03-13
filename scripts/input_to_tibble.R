# Bind all the inidividual csv files to tibbles, so we have a complete table
# for each subject
read_tennis_data <- function(year,
                             file = "atp_matches",
                             subdir = "tennis_atp") {
  readr::read_csv(
    sprintf(
      "~/Documents/R_packages/tennis/data_input/%s/%s_%d.csv",
       subdir, file, year))
}

atp_matches <- purrr::map(1968:2017, read_tennis_data) %>% do.call("rbind", .)
devtools::use_data(atp_matches)

atp_matches_futures <-
  purrr::map(1991:2017, read_tennis_data, file = "atp_matches_futures") %>%
  do.call("rbind", .)
devtools::use_data(atp_matches_futures)

atp_matches_qual_chal <-
  purrr::map(1991:2017, read_tennis_data, file = "atp_matches_qual_chall") %>%
  do.call("rbind", .)
devtools::use_data(atp_matches_qual_chal)


atp_players <- readr::read_csv(
  "~/Documents/R_packages/tennis/data_input/tennis_atp/atp_players.csv",
  col_names = FALSE)
