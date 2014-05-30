#' Generate random coefficients
#' 
#' Generates random coefficients for profit function in between -1 and 1
#' @param nodes The number of places to divide the budget among
#' @return Returns a vector of the random coefficients (same length as profit function terms)
#' @export

profit_coeff <- function (nodes){     
        sum <- 0
        for (i in 1:nodes -1){
                sum <- sum + i
        }
        number_of_coeff <- 2*nodes + sum
        coeff_vec <- c()
        for (i in 1:number_of_coeff){
                new_coeff <- runif (1, min = -1, max = 1)
                coeff_vec <- c(coeff_vec, new_coeff)
        }
        return (coeff_vec)
}

