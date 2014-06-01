\name{median_ascent}
\alias{median_ascent}
\title{Pick median ascent}
\usage{
median_ascent(neighbors, neighbor_profit, point_profit)
}
\arguments{
  \item{neighbors}{A matrix of the neighboring points to
  the current point}

  \item{neighbor_profit}{A vector of the profits for each
  neighboring point}

  \item{point_profit}{The profit of the current point, or
  budget allocation}
}
\value{
Returns the next point (as chosen by the median stratgey)
}
\description{
Takes neighboring points, the profits of those points, and
the profit of the original point to determine the next
point by choosing the median among the points that have
higher profits
}

