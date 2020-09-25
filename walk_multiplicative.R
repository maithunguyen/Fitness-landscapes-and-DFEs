library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)
numb_start <- 10
npool <- seq(50,250,50)
pathlength <- matrix(ncol = 4, nrow = numb_start*length(npool))
colnames(pathlength) <- c("n","fstart","steps","fend")
lambda <- 4
knight <- 1
for (n in npool) {
  #n <- 15 #genome size
  m <- 35 #genotypes sample
  fd <- 0.2 #lethal fraction
  fn <- 0.3 #neutral fraction
  fb <- 1 - fd -fn #beneficial fraction
  wref <- 2 #reference reproductivity
  numb_fd <- floor(n*fd)
  numb_fn <- floor(n*fn)
  numb_fb <- n - numb_fn - numb_fd
  b_lambda <- lambda*n
  d_lambda <- b_lambda
  dummy <- -rexp(1000,d_lambda) #negative exponential distribution
  dummy <- dummy[which(dummy > -1)][1:numb_fd] #select only values > -1
  s <- c(dummy[1:numb_fd],rep(0,numb_fn),rexp(numb_fb,b_lambda))
  s <- s+1
  
  #sample genotypes
  genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
  genotype_pool <- as.data.frame(genotype_pool)
  genotype_pool <- genotype_pool %>% distinct() #select only unique rows
  genotype_pool <- t(genotype_pool)
  genotype_pool <- as.matrix(genotype_pool)
  biglist <- list()

  id <- 1
  start_points <- sample(1:(m-5),numb_start)
  for (i in start_points) {
    fit <- matrix(ncol = n+1, nrow = n)
    gene <- genotype_pool[,i]
    
    #calculate fitness of each
    current_fitness <- wref*prod(s^gene)
    
    #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
    
    neighbors_fitness <- c()
    tab <- matrix(nrow = n, ncol = n)
    for (j in 1:n) {
      
      #take genotype i
      mut <- gene
      
      #mutate position j
      mut[j] <- 1 - mut[j] 
      
      #save in column j of matrix tab
      tab[,j] <- mut 
    }
    
    #matrix store all neighbor mutation of Genotype i from genotype_pool
    #each column = 1 neighbor
    neighbors_fitness <- wref*apply(s^tab,2,prod)
    position <- which(neighbors_fitness > current_fitness)
    position <- rep(position,2) #because sample function only works with vector of min length 2
    fit[1,1] <- current_fitness
    fit[1,2:(n+1)] <- neighbors_fitness
    k <- 2
    while (length(position) > 0) {
      gene <- tab[,sample(position,1)]
      current_fitness <- wref*prod(s^gene)
      neighbors_fitness <- c()
      tab <- matrix(nrow = n, ncol = n)
      for (j in 1:n) {
        mut <- gene
        mut[j] <- 1 - mut[j] 
        tab[,j] <- mut 
      }
      neighbors_fitness <- wref*apply(s^tab,2,prod)
      position <- which(neighbors_fitness > current_fitness)
      position <- rep(position,2)
      fit[k,1] <- current_fitness
      fit[k,2:(n+1)] <- neighbors_fitness
      k <- k+1
    }
    fit <- fit[1:(k-1),,drop = F]
    biglist[[id]] <- fit
    id <- id + 1
    pathlength[knight,1] <- n
    pathlength[knight,2] <- fit[1,1]
    pathlength[knight,3] <- nrow(fit)
    pathlength[knight,4] <- fit[nrow(fit),1]
    knight <- knight + 1
  }
  saveRDS(biglist, file = paste("multi_","n_", as.character(n),".rds", sep = ""))
}

saveRDS(pathlength, file = "pathlengths_multiplicative.rds")


