#' Hillclimb a landscape using steepest ascent
#' 
#' Takes a landscape object and a point (or budget allocation) and iterates the steepest ascent function to find or approach a local maxima
#' @param lanscape A landscape object (previously instantiated)
#' @param point A specific point on the landscape (a.k.a a specific allocation of the budget over the number of nodes)
#' @return A matrix of the hillclimbing sequence (which points the function jumped to on each iteration)
#' @export
#' 
hillclimb <- function (landscape, point, ascent_type){
        next_point <- c(5)
        sequence <- matrix(ncol = landscape@nodes)
        for (i in 1:100){
                if (next_point == "!"){
                        cat(sprintf("after the %.f iteration", i))
                        break  
                }
                else if (identical(ascent_type, steepest_ascent) == TRUE){
                        next_point <- steepest_ascent(neighbors(landscape@nodes, point), profit_neighbors(landscape,neighbors(landscape@nodes, point)), profit(landscape@nodes, point, landscape@coeff))
                }
                else if (identical(ascent_type, median_ascent) == TRUE){
                        next_point <- median_ascent(neighbors(landscape@nodes, point), profit_neighbors(landscape,neighbors(landscape@nodes, point)), profit(landscape@nodes, point, landscape@coeff))             
                }
                else if (identical(ascent_type, least_ascent) == TRUE){
                        next_point <- least_ascent(neighbors(landscape@nodes, point), profit_neighbors(landscape,neighbors(landscape@nodes, point)), profit(landscape@nodes, point, landscape@coeff))
                }
                sequence <- rbind(sequence, as.numeric(next_point))
                point <- next_point
        }
        print(profit(landscape@nodes, point, landscape@coeff))
        dimnames(sequence) <-list(rep("", dim(sequence)[1]), rep("", dim(sequence)[2]))
        sequence
        valid_sequence <- sequence[complete.cases(sequence),]
        cat("Hillclimbing sequence:")
        return(valid_sequence)
}