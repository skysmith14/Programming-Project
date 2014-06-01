\name{profit}
\alias{profit}
\title{Creates a profit function}
\usage{
profit(nodes, budget_A, coeff_vec)
}
\arguments{
  \item{nodes}{Number of places to divide budget amongst}

  \item{budget_A}{The division of budget across the nodes}

  \item{coeff_vec}{The vector of random coefficients to be
  multiplied by the profit function}
}
\value{
Returns a vector containing the point's linear terms,
squared terms, and cross products
}
\description{
Takes the number of nodes, budget allocation, and randomd
coefficient vector to determine a profit value for that
particular point
}

