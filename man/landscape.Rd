\name{landscape}
\alias{landscape}
\title{Create a landscape}
\usage{
landscape(nodes, budget, coeff = profit_coeff(nodes), unit)
}
\arguments{
  \item{nodes}{The number of places to divide the budget
  among}

  \item{budget}{The amount of money to invest}
}
\value{
budget allocation for each node in the form of a vector
(each spot in vector correlates to a budget allocation for
a node)
}
\description{
Takes the number of nodes and the budget and return the
budget allocation for each node
}

