#' Hillclimb a landscape using steepest ascent
#' 
#' Takes a landscape object and a point (or budget allocation) and iterates the steepest ascent function to find or approach a local maxima
#' @param lanscape A landscape object (previously instantiated)
#' @param point A specific point on the landscape (a.k.a a specific allocation of the budget over the number of nodes)
#' @return A matrix of the hillclimbing sequence (which points the function jumped to on each iteration)
#' @export
#' 
hillclimb <- function (landscape, point){
        sequence <- matrix(ncol = 4)
        for (i in 1:20){
                next_point <- steepest_ascent(neighbors(landscape@nodes, point), profit_neighbors(landscape3,neighbors(landscape@nodes, point)), profit(landscape@nodes, point, landscape@coeff))
                #print("next point")
                #print(next_point)
                if (class(next_point) == "character"){
                        cat(sprintf("after the %.f iteration", i))
                        break
                }
                else{
                        sequence <- rbind(sequence, next_point)
                        point <- next_point       
                }
                 
        }
        dimnames(sequence) <-list(rep("", dim(sequence)[1]), rep("", dim(sequence)[2]))
        sequence
        valid_sequence <- sequence[complete.cases(sequence),]
        print(" ")
        cat("Hillclimbing sequence:")
        return(valid_sequence)
}