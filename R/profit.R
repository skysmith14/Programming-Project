#' Creates a profit function
#' 
#' Takes the number of nodes, budget allocation, and randomd coefficient vector to determine a profit value for that particular point
#' @param nodes Number of places to divide budget amongst
#' @param budget_A The division of budget across the nodes
#' @param coeff_vec The vector of random coefficients to be multiplied by the profit function
#' @return Returns a vector containing the point's linear terms, squared terms, and cross products
#' @export

profit <- function(nodes, budget_A, coeff_vec){     
        profit_sum <- c()
        for (i in 1:nodes){
                new_val <- budget_A[i]
                profit_sum <- c(profit_sum, new_val)
        }
        for (i in 1:nodes){
                new_val <- (budget_A[i])^2
                profit_sum <- c(profit_sum, new_val)        
        }
        for (i in 1:(nodes-1)){
                for (z in (i+1):nodes){
                        new_val <- (budget_A[i]*budget_A[z])
                        profit_sum <- c(profit_sum, new_val)
                }
        }
        profit <- sum(profit_sum*coeff_vec) 
        return(profit)
}

