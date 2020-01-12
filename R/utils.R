libs <- function() {
  library(tidyverse)
  library(drake)
  library(recipes)
}

has_cols <- function(x, ...) {
  stopifnot(is.data.frame(x))
  cols <- list(...) %>% unlist()
  not_found <- setdiff(cols, colnames(x))
  if (length(not_found) > 0) {
    stop("Following column not in x: ", not_found[1])
  }
}

script_and_test <- function(name) {
  usethis::use_r(name)
  usethis::use_test(name)
}
