#' Creates a profit function
#' 
#' Take the budget allocation and creates complex landscape for that particular point
#' @param nodes Number of places to divide budget amongst
#' @param budget_A The division of budget across the nodes
#' @return Returns a vector containing the point's linear terms, squared terms, and cross products
#' @export

profit <- function(nodes, budget_A){     
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
        profit <- profit_sum
}

