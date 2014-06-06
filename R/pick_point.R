#' Select Random Point (or budget allocation)
#' 
#' Takes budget and nodes to generate a random distrubtion of the budget over the nodes
#' @param nodes The number of places to divide the budget amongst
#' @param budget The amount of money to spend across the nodes
#' @return Returns a vector of the allocation of budget across the given nodes
#' @export

pick_point <- function (nodes, budget){
        current <- sample(0:budget, 1)
        budget_allocation <- c()
        total <- 0
        for (i in 1:nodes){
                total <- total + current
                budget_allocation <- c(budget_allocation, current)
                if (i == nodes-1 || total == budget){
                        current <- budget - total
                }
                else{
                        current <- sample(0:(budget-total), 1)   
                }
        }
        return(budget_allocation)
}