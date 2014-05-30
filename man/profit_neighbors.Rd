\name{profit_neighbors}
\alias{profit_neighbors}
\title{Find profit for neighbors}
\usage{
profit_neighbors(landscape, neighbors)
}
\arguments{
  \item{landscape}{A landscape object (previously
  instantiated)}

  \item{neighbors}{A matrix of neighboring points}
}
\value{
Returns a vector of profits for each neighboring point
}
\description{
Takes a landscape object and a matrix of neigboring points,
and determines the profit value for each of the neighboring
points
}

