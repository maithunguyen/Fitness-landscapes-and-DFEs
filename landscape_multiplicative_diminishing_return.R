library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

localoptimalmatrix <- matrix(NA, ncol = 11, nrow = 10000)
colnames(localoptimalmatrix) <- c("genome_size","numb_fn","numb_fb","numb_fd",
                                  "fn","fb","fd","dimin","sample_size","local_optima","local_optima_strict")
pawnpower <- 1

for (n in c(5,7,9,10,11,13)) {
  for (fd in c(0.2,0.3,0.4,0.5,0.6)) {
    #n <- 11 #genome size
    m <- 70000 #genotypes sample
    #fd <- 0.3 #lethal fraction
    fn <- 0.3 #neutral fraction
    fb <- 1 - fd -fn #beneficial fraction
    wref <- 2 #reference reproductivity
    
    numb_fd <- round(n*fd)
    numb_fn <- round(n*fn)
    numb_fb <- n - numb_fn - numb_fd
    b_lambda <- 4
    d_lambda <- b_lambda
    dummy <- -rexp(1000,d_lambda) #negative exponential distribution
    dummy <- dummy[which(dummy > -1)][1:numb_fd] #select only values > -1
    s <- c(dummy[1:numb_fd],rep(0,numb_fn),rexp(numb_fb,b_lambda))
    s <- s+1
    s <- round(s, digits = 2)
    
    #sample genotypes
    genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
    genotype_pool <- as.data.frame(genotype_pool)
    #install.packages("tidyverse")
    library(tidyverse)
    genotype_pool <- genotype_pool %>% distinct() #select only unique rows
    
    #calculate fitness of each
    genotype_pool <- t(genotype_pool)
    
    #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
    genotype_pool <- as.matrix(genotype_pool)
    neighbors <- list()
    for (i in 1:ncol(genotype_pool)) {
      tab <- matrix(nrow = n, ncol = n)
      for (j in 1:n) {
        
        #take genotype i
        mut <- genotype_pool[,i]
        
        #mutate position j
        mut[j] <- 1 - mut[j] 
        
        #save in column j of matrix tab
        tab[,j] <- mut 
      }
      
      #matrix store all neighbor mutation of Genotype i from genotype_pool
      #each column = 1 neighbor
      neighbors[[i]] <- tab 
    }
    
    for (dimin in c(seq(0,1,0.05))) {
      number_of_mutation <- colSums(genotype_pool)
      individuals_with_multiple_mutation <- which(number_of_mutation > 1)
      #producing children as poisson process with mean calculated from fitness, more realistic than round the fitness
      children_individuals <- wref*(apply(s^genotype_pool,2,prod))
      children_individuals[individuals_with_multiple_mutation] <- (dimin^(number_of_mutation[individuals_with_multiple_mutation]-1))*children_individuals[individuals_with_multiple_mutation]
      
      current_fitness <- children_individuals
      
      #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
      neighbors_fitness <- matrix(nrow = n, ncol = ncol(genotype_pool))
      for (f in 1:ncol(genotype_pool)) {
        number_of_mutation <- colSums(neighbors[[f]])
        individuals_with_multiple_mutation <- which(number_of_mutation > 1)
        #producing children as poisson process with mean calculated from fitness, more realistic than round the fitness
        children_individuals <- wref*(apply(s^neighbors[[f]],2,prod))
        children_individuals[individuals_with_multiple_mutation] <- (dimin^(number_of_mutation[individuals_with_multiple_mutation]-1))*children_individuals[individuals_with_multiple_mutation]
        
        neighbors_fitness[,f] <- children_individuals
      }
      
      localoptimalmatrix[pawnpower,] <- c(n,
                                          length(which(s == 1)),
                                          length(which(s > 1)),
                                          length(which(s < 1)),
                                          fn,fb,fd,dimin,
                                          length(current_fitness),
                                          length(which(current_fitness >= apply(neighbors_fitness,2,max))),
                                          length(which(current_fitness > apply(neighbors_fitness,2,max)))
                                          )
      pawnpower <- pawnpower + 1
    }
  }
}



localoptimalmatrix_multip_return <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_multip_return, file = "localoptimalmatrix_multip_return.rds")
#ggpairs(localoptimalmatrix_multip_return, mapping = NULL, columns = 1:ncol(localoptimalmatrix_multip_return), title = "diminishing multiplicative landscape")

eq <- which(round(current_fitness, digits = 2) - apply(round(neighbors_fitness, digits = 2), 2, max) == 0)
neq <- which(round(current_fitness, digits = 2) - apply(round(neighbors_fitness, digits = 2), 2, max) > 0)
genotype_pool[,eq]
current_fitness[eq]
genotype_pool[,neq]
current_fitness[neq]
