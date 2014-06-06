#' Find profit of neighbors
#' 
#' Takes the landscape and neighboring points, and finds the profits for those points
#' @param landscape The economic landscape object
#' @param nieghbors The vector of neighboring points
#' @return returns the vector of profits for each neighboring point
#' @export 
#' 
profit_neighbors <- function(landscape, neighbors){
        profit_vec <- c()
        for (i in 1:nrow(neighbors)){
                new_profit <- profit(landscape@nodes, neighbors[i,], landscape@coeff)
                profit_vec <- c(profit_vec, new_profit)
        }
        return (profit_vec)
}