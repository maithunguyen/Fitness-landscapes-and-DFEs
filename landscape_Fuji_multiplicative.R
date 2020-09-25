library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)
library(magrittr)

localoptimalmatrix <- matrix(NA, ncol = 8, nrow = 10000)
colnames(localoptimalmatrix) <- c("genome_size","numb_fn","numb_fb","sd","ratio","sample_size","local_optima","local_optima_strict")
pawnpower <- 1

for (n in c(5,7,9,10,11,13)) {
  #n <- 5 #genome size
  m <- 10000 #genotypes sample
  fd <- 0.2 #lethal fraction
  fn <- 0.3 #neutral fraction
  fb <- 1 - fd -fn #beneficial fraction
  wref <- 2 #reference reproductivity
  meanlog <- 1
  #sd <- 1
  
  numb_fd <- floor(n*fd)
  numb_fn <- floor(n*fn)
  numb_fb <- n - numb_fn - numb_fd
  b_lambda <- 4
  d_lambda <- b_lambda
  dummy <- -rexp(1000,d_lambda) #negative exponential distribution
  dummy <- dummy[which(dummy > -1)][1:numb_fd] #select only values > -1
  s <- c(dummy[1:numb_fd],rep(0,numb_fn),rexp(numb_fb,b_lambda))
  s <- s+1
  
  #sample genotypes
  genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
  genotype_pool <- as.data.frame(genotype_pool)
  genotype_pool <- genotype_pool %>% distinct() #select only unique rows
  
  
  #calculate fitness of each
  genotype_pool <- t(genotype_pool)
  
  for (ratio in c(0.0001,0.001,seq(0.01,1.9,0.06))) {
    sd <- 1/b_lambda/ratio
    bishop <- 1
    for (bishop in 1:100) {
    current_fitness <- wref*apply(s^genotype_pool,2,prod)
    
    # Nonadditive term
    noise <- rlnorm(length(current_fitness),meanlog,sd) #lognormal distribution
    
    current_fitness <- current_fitness * noise
    
    #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
    genotype_pool <- as.matrix(genotype_pool)
    neighbors <- list()
    neighbors_fitness <- matrix(nrow = n, ncol = ncol(genotype_pool))
    for (i in 1:length(current_fitness)) {
      tab <- matrix(nrow = n, ncol = n)
      for (j in 1:n) {
        
        #take genotype i
        mut <- genotype_pool[,i]
        
        #mutate position j
        mut[j] <- 1 - mut[j] 
        
        #save in column j of matrix tab
        tab[,j] <- mut 
        
        position = which(colSums(mut == genotype_pool) == n)
        if (length(position) == 1){
          neighbors_fitness[j,i] <- wref*prod(s^mut)*noise[position]
        # if (length(which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)) == 1){ #1 genotype in the pool is identical to this neighbor
        #   neighbors_fitness[j,i] <- wref*prod(s^mut)*noise[which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)]
        }else{ #not in the genotype pool yet
          genotype_pool <- cbind(genotype_pool,mut)
          
          newnoise <- rlnorm(1,meanlog,sd) 
          noise <- c(noise,newnoise)
          neighbors_fitness[j,i] <- wref*prod(s^mut)*newnoise
        }
      }
      
      #matrix store all neighbor mutation of Genotype i from genotype_pool
      #each column = 1 neighbor
      neighbors[[i]] <- tab 
      
    }

    localoptimalmatrix[pawnpower,] <- c(n,
                                        length(which(s == 1)),
                                        length(which(s > 1)),
                                        sd,
                                        ratio,
                                        length(current_fitness),
                                        length(which(current_fitness >= apply(neighbors_fitness,2,max))),
                                        length(which(current_fitness > apply(neighbors_fitness,2,max)))) 
    pawnpower <- pawnpower + 1
    bishop <- bishop + 1
    }
  }
}


localoptimalmatrix_fuji_multiplicative <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_fuji_multiplicative, file = "localoptimalmatrix_fuji_multiplicative_ratio.rds")

