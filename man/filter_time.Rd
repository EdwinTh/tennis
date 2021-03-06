% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{filter_time}
\alias{filter_time}
\title{Filter Tennis Data on Time}
\usage{
filter_time(x, from, to = from)
}
\arguments{
\item{x}{A data frame from the \code{tennis} package.}

\item{from}{A vector of length one that indicates the starting point.
See Details.}

\item{to}{A vector of length one that indicates the end point.
See Details.}
}
\value{
The subset in \code{x} of all the matches that were played
between \code{from} and \code{to}.
}
\description{
Return all matches in \code{x} that were played between two dates.
Matches played on \code{to} or \code{from} are included in te returned data.
}
\details{
The values for \code{from} and \code{to} are compared to the
integer field \code{tourney_date} in \code{x}. When they are nonnumeric,
for instance of class \code{Date}, they are cleaned and coerced to integer.
\code{from} and \code{to} may contain less than eight characters, they will
padded if necesarry.
}
\examples{
# get all matches in 2016
filter_time(atp_matches, 2016)

# get all matches from tourneys that start in June 2016
filter_time(atp_matches, 201606)

# get all matches from tourneys started between
# June 2015 and May 2016
filter_time(atp_matches, "2015-06", "2016-05")
}
