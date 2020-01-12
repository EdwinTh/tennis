atp_plan <- drake_plan(
  atp_matchups = make_player_sets(),
  atp_glm_sets = features_to_matchups(atp_matchups)
)

make(atp_plan, prework = devtools::load_all())
