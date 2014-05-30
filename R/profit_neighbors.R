#' Find profit for neighbors
#' 
#' Takes a landscape object and a matrix of neigboring points, and determines the profit value for each of the neighboring points
#' @param landscape A landscape object (previously instantiated)
#' @param neighbors A matrix of neighboring points
#' @return Returns a vector of profits for each neighboring point
#' @export
#' 
profit_neighbors <- function(landscape, neighbors){
        profit_vec <- c()
        for (i in 1:nrow(neighbors)){
                new_profit <- profit(landscape@nodes, neighbors[i,], landscape@coeff)
                profit_vec <- c(profit_vec, new_profit)
        }
        #print(profit_vec)
        return (profit_vec)
}