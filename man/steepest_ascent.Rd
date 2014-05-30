\name{steepest_ascent}
\alias{steepest_ascent}
\title{Pick highest Ascent}
\usage{
<<<<<<< HEAD
steepest_ascent(neighbors)
}
\arguments{
  \item{neighbors}{The matrix of neighboring points}
=======
steepest_ascent(neighbors, neighbor_profits, point_profit)
}
\arguments{
  \item{neighbors}{The matrix of neighboring points}

  \item{neighbor_profits}{The vector of the profits of each
  neighboring point}

  \item{point_profit}{The profit of the current point}
>>>>>>> a821ca52cf2cf842bd56e93d2edd2897fccd0362
}
\value{
The neighbor with the highest profit
}
\description{
<<<<<<< HEAD
Chooses the neighbor having the highest profit
=======
Chooses the neighboring point that has the highest profit
(or the current point if its profit is higher than that of
any of its neighbors)
>>>>>>> a821ca52cf2cf842bd56e93d2edd2897fccd0362
}

