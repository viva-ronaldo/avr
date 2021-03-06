% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stv.R
\name{stv}
\alias{stv}
\title{Single transferable vote}
\usage{
stv(
  votes,
  nseats,
  use_fps_for_final_tie = TRUE,
  transfer_surplus = TRUE,
  verbose = FALSE,
  report = FALSE,
  report_path = ifelse(report, "stv_single_report.html", NULL),
  getTable = ifelse(report, TRUE, FALSE),
  getMatrix = FALSE
)
}
\arguments{
\item{votes}{A list of order-of-preference vote vectors, or a list of ballot
objects.}

\item{nseats}{How many seats to fill.}

\item{use_fps_for_final_tie}{Use number of first preference votes to settle a tie on the last round of the count? If not, settles the tie randomly.}

\item{transfer_surplus}{Logical indicating whether or not to transfer the surplus votes when a candidate is elected with votes in excess of the quota. This should be TRUE for standard STV, but in custom situations it may be more desirable to drop the excess votes.}

\item{verbose}{Print out intermediate information or not.}

\item{report}{Generate an HTML report of the vote.}

\item{report_path}{When \code{report} is TRUE, the filepath to which to save the generated report.}

\item{getTable}{Calculate and return the count table. Must be TRUE if \code{report} is TRUE.}

\item{getMatrix}{Calculate and return the transfer matrix.}
}
\value{
An STV object, containing:
  \describe{
    \item{votes:}{the list of ballots}
    \item{nballots:}{the number of ballots cast}
    \item{candidates:}{the list of candidates}
    \item{nseats:}{the number of seats available}
    \item{quota:}{the STV quota}
    \item{winners:}{the candidates that are elected}
    \item{used_random:}{logical indicating if it was necessary to randomly break ties}
    \item{transfer_matrix:}{a matrix recording the vote transfers that occurred during this count}
    \item{count_table:}{a data.frame recording the number of votes for each candidate at each round in the count, and whether or not the candidate was elected}
  }
}
\description{
Single transferable vote
}
\examples{
votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")
)

stv(votes, 2)

map <- c("e", "f", "i", "u", "w")
votes <- list(
  ballot(0, 3, 1, 2, 0, map = map),
  ballot(0, 3, 1, 2, 0, map = map),
  ballot(2, 0, 0, 0, 1, map = map),
  ballot(0, 3, 1, 2, 0, map = map)
)
stv(votes, 2)
}
