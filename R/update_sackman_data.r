#' Jeff Sackmann kindly provides all tennis data on
#'   his github. It is updated a few times a year.
#'   This function downloads the latest data and
#'   stores it data_input.
#' @param src Which source should be downloaded?
#' @rertun None, called for its side effects.
update_sackmann_data <- function(src = c("both" ,"atp", "wta")) {
  src <- match.arg(src)

  if (src != "wta") {
    setwd("data_input")
    remove_if_exists("tennis_atp")
    system("git clone https://github.com/JeffSackmann/tennis_atp")
  }

  if (src != "atp") {
    setwd("data_input")
    remove_if_exists("tennis_wta")
    system("git clone https://github.com/JeffSackmann/tennis_wta")
  }
}

remove_if_exists <- function(file_path) {
  if (file.exists(file_path)) {
    sys_cmnd <- glue::glue("rm -r {file_path}")
    system(sys_cmnd)
  }
}

#' The downloaded data are read and bound to
#'   one single set and stored within the package.
raw_data_to_set <- function(src = c("both" ,"atp", "wta"),
                            first_year = 2000) {
  src <- match.arg(src)
  if (src != "wta") {
    all_files <- list.files("data_input/tennis_atp/")
    all_files <- all_files[stringr::str_detect(all_files, "atp_matches*")] %>%
      filter_from_year(first_year)
    atp_matches <- purrr::map_dfr(file.path("data_input/tennis_atp/", all_files),
                                  ~readr::read_csv(.x, col_types = readr::cols(.default = "c")))
    saveRDS(atp_matches, "data_processed/atp.Rds")
  }

  if (src != "atp") {
    all_files <- list.files("data_input/tennis_wta/")
    all_files <- all_files[stringr::str_detect(all_files, "wta_matches*")] %>%
      filter_from_year(2003) # incomplete data earlier
    wta_matches <- purrr::map_dfr(file.path("data_input/tennis_wta/", all_files),
                                  ~readr::read_csv(.x, col_types = readr::cols(.default = "c")))
    saveRDS(wta_matches, "data_processed/wta.Rds")
  }
}

filter_from_year <- function(x, yr) {
  ind <- stringr::str_match(x, "\\d{4}")[,1] %>%
    as.numeric() %>%
    `>=`(yr)
  x[ind]
}
