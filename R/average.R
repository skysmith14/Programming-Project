#' Average strategy results
#' 
#' Takes a landscape and finds the average local maxima profit for each strategy after 100 iterations
#' @param landscape The economic landscape
#' @return Returns the vector of averages for each hillclimbing strategy
#' @export
#' 
average <- function (landscape){
        matrix1 <- matrix(ncol = 3)
        colnames(matrix1) <- c("steepest ascent", "median ascent", "least ascent")
        for (i in 1:1000){
                rand_point <- pick_point(landscape@nodes, landscape@budget)
                steep_max <- hillclimb(landscape, rand_point, steepest_ascent)
                med_max <- hillclimb(landscape, rand_point, median_ascent)
                least_max <- hillclimb(landscape, rand_point, least_ascent)
                vec <- c(steep_max, med_max, least_max)
                matrix1 <- rbind(matrix1, vec)
        }
        matrix1 <- matrix1[complete.cases(matrix1),]
        means <- colMeans(matrix1, na.rm = TRUE)
        return(means)
        #new_matrix <- matrix(ncol = 3)
        #colnames(new_matrix) <- c("steepest ascent", "median ascent", "least ascent")
        #rownames(new_matrix) <- c(1)
        #new_matrix <- rbind(new_matrix, means)
        #new_matrix <- new_matrix[complete.cases(new_matrix),]
        #print(new_matrix)
}