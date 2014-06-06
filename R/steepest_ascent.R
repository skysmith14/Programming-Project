#' Pick highest Ascent
#' 
#' Chooses the neighboring point that has the highest profit (or the current point if its profit is higher than that of any of its neighbors)
#' @param neighbors The matrix of neighboring points
#' @param neighbor_profits The vector of the profits of each neighboring point
#' @param point_profit The profit of the current point
#' @return The neighbor with the highest profit
#' @export

steepest_ascent <- function (neighbors, neighbor_prof, point_profit){
       pos_of_highest <- which.max(neighbor_prof)
       if (neighbor_prof[pos_of_highest] < point_profit){
               return(round(point_profit, 2))
       }
       else{
               next_point <- neighbors[pos_of_highest,]
               return(next_point)    
       }
}