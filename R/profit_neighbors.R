profit_neighbors <- function(landscape, neighbors){
        profit_vec <- c()
        for (i in 1:nrow(neighbors)){
                new_profit <- profit(landscape@nodes, neighbors[i,], landscape@coeff)
                profit_vec <- c(profit_vec, new_profit)
        }
        #print(profit_vec)
        return (profit_vec)
}