#' Create a landscape
#' 
#' Takes the number of nodes and the budget and return the budget allocation for each node
#' @param nodes The number of places to divide the budget among
#' @param budget The amount of money to invest
#' @return budget allocation for each node in the form of a vector (each spot in vector correlates to a budget allocation for a node)

landscape <- setClass("landscape", slots = c(nodes = "numeric", budget = "numeric", coeff = "numeric", unit = "numeric"))


landscape <- function (nodes, budget, coeff = profit_coeff(nodes), unit){
        point <- pick_point(nodes,budget) # find rand. point (a.k.a rand. budget allocation)
        print(point)
        print("*")
        
        profit_sum <- profit(nodes, point) # find profit 
        print(profit_sum)
        print("*")
        
        total_profit <- sum(profit_sum*coeff) # multiply by coeff's & sum up
        print(total_profit)
        print("*")
        
        neighbors <- neighbors(nodes, point) # find neighbors
        print(neighbors)
        print("*")
        
        vec_profit_neighbors <- c()
        for (i in 1:(nrow(neighbors))){
                profit_neigh <- profit(nodes, neighbors[i,])
                vec_profit_neighbors <- c(vec_profit_neighbors, profit_neigh)
        }
        print(vec_profit_neighbors)
        print("*")
        
        optimal <- steepest_ascent(vec_profit_neighbors) #picks neighbor with highest profit
        print(vec_profit_neighbors[optimal])
}