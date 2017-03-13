#' All Matches in the Open Era
#'
#' Dataset of all the atp matches played since the start of the open era (1968).
#' @format A data frame with 164028 rows and 49 variables:
#' \describe{
#' \item{tourney_id}{Tournament year id, combination of the year and the id.}
#' \item{tourney_name}{Tournament name.}
#' \item{surface}{The surface the tournament was played on.}
#' \item{draw_size}{The size of the draw (number of players competing)}
#' \item{tourney_level}{A = regular atp, C = challenger, D = davis cup, F = tour finals, G = grand slam, M = master series}
#' \item{tourney_date}{Start date of the tournament.}
#' \item{match_num}{Sequence number for the matches played at the tournament in the given year.}
#' \item{winner_id}{ID of the player that won the match.}
#' \item{winner_seed}{Tournament seeding of the player that won the match.}
#' \item{winner_entry}{}
#' \item{winner_name}{}
#' \item{winner_hand}{}
#' \item{winner_ht}{}
#' \item{winner_ioc}{}
#' \item{winner_age}{}
#' \item{winner_rank}{}
#' \item{winner_rank_points}{}
#' \item{loser_id}{}
#' \item{loser_seed}{}
#' \item{loser_entry}{}
#' \item{loser_name}{}
#' \item{loser_ht}{}
#' \item{loser_ioc}{}
#' \item{loser_age}{}
#' \item{loser_rank}{}
#' \item{loser_rank_points}{}
#' \item{score}{}
#' \item{best_of}{}
#' \item{round}{}
#' \item{minutes}{}
#' \item{w_ace}{}
#' \item{w_df}{}
#' \item{w_svpt}{}
#' \item{w_1stIn}{}
#' \item{w_1stWon}{}
#' \item{w_2ndWon}{}
#' \item{w_SvGms}{}
#' \item{w_bpSaved}{}
#' \item{w_bpFaced}{}
#' \item{l_ace}{}
#' \item{l_df}{}
#' \item{l_svpt}{}
#' \item{l_1stIn}{}
#' \item{l_1stWon}{}
#' \item{l_2ndWon}{}
#' \item{l_SvGms}{}
#' \item{l_bpSaved}{}
#' \item{l_bpFaced}{}

#' @source \url{https://github.com/JeffSackmann/tennis_atp}
"emergency"
