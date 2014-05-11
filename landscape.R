landscape <- function (nodes, budget){
        #pre: receives number of nodes, budget
        #post: returns budget allocation for each node in the form of a vector
        #(each spot in vector correlates to a budget allocation for a node)
        
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


profit <- function(nodes){
        #pre: receives number of nodes
        #post: returns the profit allocation per dollar (or unit) of the budget 
        # for each node
        
        profit_allocation <- c()
        for (i in 1:nodes){
                value <- runif(1, 1, 100)
                profit_allocation <- c(profit_allocation, value)
        }
        return (profit_allocation)
}


profit_landscape <- function(budget_A, profit_A){
        #pre: receives budget allocation vector and profit allocation vector
        #post: returns total profit for that landscape
        
        #print(budget_A)
        #print(profit_A)
        total_vec <- (budget_A)*(profit_A)
        length <- length(budget_A)
        total <- 0
        for (i in 1:length){
                #print(total_vec[i])
                total <- total + total_vec[i]
        }
        return(total)
}


neighbors <- function (nodes, budget_A){
        #pre: receives number of nodes and vector of budget allocation
        #post: returns vector of neighboring budget allocation vectors
        grand <- c()
        for (i in 1:(nodes-1)){
                new1 <- budget_A
                new1[i] <- new1[i] + 1
                new1[i+1] <- new1[i+1] - 1
                #print(new1)
                new2 <- budget_A
                new2[i] <- new2[i] - 1
                new2[i+1] <- new2[i+1] + 1
                #print(new2)
                grand <- c(grand, new1, new2)
        }
        print(grand)
}

pick_neighbors <- function (neighbor1, neighbor2){
        #pre: receives two sets of neighbor budget allocations
        #post: returns a randomly chosen set
        num <- sample(1:2, 1)
        print(num)
        if (num == 1){
                return(neighbor1)
        }
        if (num == 2){
                return(neighbor2)
        }
}