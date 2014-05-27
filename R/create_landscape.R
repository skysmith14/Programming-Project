pick_point <- function (nodes, budget){
        current <- sample(0:budget, 1)
        budget_allocation <- c()
        total <- 0
        for (i in 1:nodes){
                total <- total + current
                budget_allocation <- c(budget_allocation, current)
                if (i == nodes-1 || total == budget){
                        current <- budget - total
                }
                else{
                        current <- sample(0:(budget-total), 1)   
                }
        }
        
        return(budget_allocation)
}