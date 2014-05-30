\docType{class}
\name{landscape-class}
\alias{landscape-class}
\title{Create a landscape}
\arguments{
  \item{nodes}{The number of places to divide the budget
  among}

  \item{budget}{The amount of money to invest}

  \item{coeff}{The vector of random coefficients for the
  profit function}

  \item{unit}{The monetary unit of transfer to define a
  "neighbor"}
}
\value{
budget allocation for each node in the form of a vector
(each spot in vector correlates to a budget allocation for
a node)
}
\description{
Takes the number of nodes, budget, random coefficient
vector, and monetary unit of transfer to create a landscape
object
}

