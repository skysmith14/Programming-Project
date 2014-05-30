#' Create a landscape
#' 
#' Takes the number of nodes, budget, random coefficient vector, and monetary unit of transfer to create a landscape object 
#' @param nodes The number of places to divide the budget among
#' @param budget The amount of money to invest
#' @param coeff The vector of random coefficients for the profit function
#' @param unit The monetary unit of transfer to define a "neighbor"
#' @return budget allocation for each node in the form of a vector (each spot in vector correlates to a budget allocation for a node)
#' @export

setClass("landscape", slots = c(nodes = "numeric", budget = "numeric", coeff = "vector", unit = "numeric"))


