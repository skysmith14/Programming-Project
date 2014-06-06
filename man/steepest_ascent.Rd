\name{steepest_ascent}
\alias{steepest_ascent}
\title{Pick highest Ascent}
\usage{
steepest_ascent(neighbors, neighbor_prof, point_profit)
}
\arguments{
  \item{neighbors}{The matrix of neighboring points}

  \item{neighbor_profits}{The vector of the profits of each
  neighboring point}

  \item{point_profit}{The profit of the current point}
}
\value{
The neighbor with the highest profit
}
\description{
Chooses the neighboring point that has the highest profit
(or the current point if its profit is higher than that of
any of its neighbors)
}

