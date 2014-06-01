#' Pick median ascent
#' 
#' Takes neighboring points, the profits of those points, and the profit of the original point to determine the next point by choosing the median among the points that have higher profits
#' @param neighbors A matrix of the neighboring points to the current point
#' @param neighbor_profit A vector of the profits for each neighboring point
#' @param point_profit The profit of the current point, or budget allocation
#' @return Returns the next point (as chosen by the median stratgey)
#' @export
#' 
median_ascent <- function (neighbors, neighbor_profit, point_profit){
        improvement <- neighbor_profit[neighbor_profit > point_profit]
        if (length(improvement) == 0){
                cat(sprintf("You've reached the local maxima of %f", point_profit))
                return("!")
        }
        else{
                median_profit <- median(improvement)
                if (length(improvement) %% 2 == 0){
                        median_profit <- neighbor_profit[which.min(abs(neighbor_profit-median_profit))]
                }
                pos <- match(median_profit, neighbor_profit)
                return (neighbors[pos,])
        }  
}
