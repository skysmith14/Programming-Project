#' Pick highest Ascent
#' 
#' Chooses the neighboring point that has the highest profit (or the current point if its profit is higher than that of any of its neighbors)
#' @param neighbors The matrix of neighboring points
#' @param neighbor_profits The vector of the profits of each neighboring point
#' @param point_profit The profit of the current point
#' @return The neighbor with the highest profit
#' @export

steepest_ascent <- function (neighbors, neighbor_profits, point_profit){
       pos_of_highest <- which.max(neighbor_profits)
       #print(neighbor_profits)
       #print(point_profit)
       #print(neighbors)
       if (neighbor_profits[pos_of_highest] < point_profit){
               cat(sprintf("You've reached the local maxima of %f", point_profit))
               return("!")
       }
       else{
               next_point <- neighbors[pos_of_highest,]
               return(next_point)    
       }
}