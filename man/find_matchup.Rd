% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/players.R
\name{find_matchup}
\alias{find_matchup}
\title{Find a matchup between two or more players}
\usage{
find_matchup(x, players_a, players_b = players_a)
}
\arguments{
\item{x}{A data frame from the \code{tennis} package.}

\item{players_a}{A character vector.}

\item{players_a}{A character vector.}
}
\value{
A subset of \code{x} with the matchups between the requested players.
}
\description{
Find the matches between two or more specific opponents. If only
\code{players_a} is specified all the matches between the players in this
set are returned. If both \code{players_a} and \code{players_b} are
specified all the matched betweena player in set a and a player in set b
are specified. Matching is case-insensitive.
}
\examples{
# Find all matches between Nadal and Federer:
find_matchup(atp_matches, "Roger Federer", "Rafael Nadal")

# Find all matches between Nadal, Djokovic, and Federer:
find_matchup(atp_matches, c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))

Find all matches that Nadal or Federer played against Djokovic or Murray:
a <- c("Roger Federer", "Rafael Nadal"); b <- c("Novak Djokovic", "Andy Murray")
find_matchup(atp_matches, a, b)

Find all matches that Nadal and Federe played against Djokovic or Murray
and all the matches they played against each other:
find_matchup(atp_matches, a, c(a, b))
}
