glm_from_match_up <- function(mu_data) {
  mu_data %>%
    add_matchup() %>%
    mutate(player1_won = as.numeric(winner_name == player1)) %>%
    glm(player1_won ~ surface, data = ., family = "binomial")
}

add_glm_to_matchups <- function(matchups) {
  func <- safely(glm_from_match_up)
  ret <- matchups %>%
    mutate(glm_mod_s = map(data, func))
  ret %>% mutate(mod_ok = map_lgl(glm_mod_s, ~is.null(.x$error))) %>%
    filter(mod_ok) %>%
    mutate(glm_mod = map(glm_mod_s, 1)) %>%
    select(-glm_mod_s, -mod_ok)
}

predict_glm <- function(matchups_glm, p1, p2, sfc = "Hard") {
  mu <- matchup_glm %>%
    filter(player1 == {{p1}}, player2 == {{p2}})
  if (nrow(mu) == 0) stop("No model for ", p1, " versus ", p2)
  try(mod <- predict(mu$glm_mod[[1]],
                     data.frame(surface = sfc),
                     se.fit = TRUE,
                     type = "response"),
      silent = TRUE
  )
  if (exists("mod")) {
    return(mod)
  } else {
    stop("New surface level, no predcitions")
  }
}

add_odds <- function(x) {
  has_cols(x, "prob", "cum_dist")
  x %>%
    mutate(odds = cum_dist / (1 - cum_dist))
}

create_99_grid <- function(fit, se) {
  tibble(prob     = seq(.01, .99, .01),
         cum_dist = qnorm(seq(.01, .99, .01), mean = fit, sd = se)) %>%
    mutate(cum_dist = ifelse(cum_dist < 0, .0000001, cum_dist),
           cum_dist = ifelse(cum_dist > 1, .99999999, cum_dist))
}
