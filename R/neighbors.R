#' Find neighbors
#' 
#' Takes the number of nodes and the budget allocation and detemines neighboring points 
#' @param nodes The number of places to divide the budget among
#' @param budget_A The allocation of the budget for each node
#' @return Returns vector of neighboring budget allocation vectors
#' @export 

neighbors <- function (nodes, budget_A){
        grand <- matrix(ncol = nodes)
        for (i in 1:nodes){
                a <- i
                new1 <- c(budget_A)
                new1[i] <- new1[i] + 1
                if (i==nodes){
                        i <- 0
                }
                new1[i+1] <- new1[i+1] - 1
                for (z in 1:nodes){
                        if (new1[z]<0){
                                new1 <- c()
                                break
                        }
                }
                new2 <- c(budget_A)
                new2[a] <- new2[a] - 1
                if (a==nodes){
                        a <- 0
                }
                new2[a+1] <- new2[a+1] + 1
                for (y in 1:nodes){
                        if (new2[y]<0){
                                new2 <- c()
                                break
                        }
                }
                grand <- rbind(grand, new1, new2)
        }
        dimnames(grand) <-list(rep("", dim(grand)[1]), rep("", dim(grand)[2]))
        grand
        valid_neighbors <- grand[complete.cases(grand),]
        #print(valid_neighbors)
        return(valid_neighbors)
}