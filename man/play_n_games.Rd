% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montyhall.R
\name{play_n_games}
\alias{play_n_games}
\title{Play Monty Hall problem game 100 times.}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns the wins and loses after 100 games.
}
\description{
`play_n_game()` runs the Monty Hall problem 100 times.
}
\details{
The game replicates the game on the TV show
  "Let's Make a Deal" 100 times to determine if it is better to
  stay or switch the door once the goat door is opened. It
  shows what is the best strategy for the game.
}
\examples{
  play_n_game()

}
