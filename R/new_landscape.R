new_landscape <- function(landscape){
        budget <- landscape@budget
        nodes <- landscape@nodes
        coeffs <- landscape@coeff
        
        point <- pick_point(nodes,budget) # find rand. point (a.k.a rand. budget allocation)
        print(point)
        print("*")
        
        profit_sum <- profit(nodes, point) # find profit 
        print(profit_sum)
        print("*")
        
        total_profit <- sum(profit_sum*coeffs) # multiply by coeff's & sum up
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