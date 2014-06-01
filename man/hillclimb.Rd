\name{hillclimb}
\alias{hillclimb}
\title{Hillclimb a landscape using steepest ascent}
\usage{
hillclimb(landscape, point, ascent_type)
}
\arguments{
  \item{lanscape}{A landscape object (previously
  instantiated)}

  \item{point}{A specific point on the landscape (a.k.a a
  specific allocation of the budget over the number of
  nodes)}
}
\value{
A matrix of the hillclimbing sequence (which points the
function jumped to on each iteration)
}
\description{
Takes a landscape object and a point (or budget allocation)
and iterates the steepest ascent function to find or
approach a local maxima
}

