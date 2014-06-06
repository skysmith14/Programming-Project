#' Pick least ascent
#' 
#' Takes neighboring points, the profits of those points, and the profit of the original point to determine the next point by choosing the smallest point above the current profit
#' @param neighbors A matrix of the neighboring points to the current point
#' @param neighbor_profit A vector of the profits for each neighboring point
#' @param point_profit The profit of the current point, or budget allocation
#' @return Returns the next point (as chosen by the least ascent stratgey)
#' @export
#' 
least_ascent <- function (neighbors, neighbor_profit, point_profit){
        improvement <- neighbor_profit[neighbor_profit > point_profit]
        if (length(improvement) == 0){
                return(round(point_profit, 2))
        }
        else{
                 new_profit <- min(improvement)
        }
        pos <- match(new_profit, neighbor_profit)
        return (neighbors[pos,])
}