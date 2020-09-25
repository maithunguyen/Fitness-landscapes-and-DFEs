library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

numb_start <- 50
npool <- seq(50,250,50)
ratiopool <- c(seq(0.01,1.5,0.05),2,3,4,5)
m <- 100 #genotypes sample
fd <- 0.5 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
meanlog <- 1
lambda <- 4
pathlength <- matrix(ncol = 7, nrow = numb_start*length(npool)*length(ratiopool))
colnames(pathlength) <- c("n","ratio","fstart","steps","fend","numb_fb","numb_fn")
knight <- 1
for (n in npool) {
    numb_fd <- round(n*fd)
    numb_fn <- round(n*fn)
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
    
  for (ratio in ratiopool) {
    sd <- 1/b_lambda/ratio
    noise <- rlnorm(ncol(genotype_pool),meanlog,sd) #lognormal distribution

    biglist <- list()
    
    id <- 1
    start_points <- sample(1:(m-5),numb_start)
    for (i in start_points) {
      fit <- matrix(ncol = n+1, nrow = n)
      gene <- genotype_pool[,i]
      #calculate fitness of each
      current_fitness <- wref*prod(s^gene)*noise[i]
      
      #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
      
      neighbors_fitness <- c()
      tab <- matrix(nrow = n, ncol = n)
      noiseneigh <- 1:n
      for (j in 1:n) {
        
        #take genotype i
        mut <- gene
        
        #mutate position j
        mut[j] <- 1 - mut[j] 
        
        #save in column j of matrix tab
        tab[,j] <- mut 
        
        #matrix store all neighbor mutation of Genotype i from genotype_pool
        #each column = 1 neighbor
        posit = which(colSums(mut == genotype_pool) == n)
        
        if (length(posit) == 1){
          neighbors_fitness[j] <- wref*prod(s^mut)*noise[posit]
          noiseneigh[j] <- noise[posit]
        }else{ #not in the genotype pool yet
          genotype_pool <- cbind(genotype_pool,mut)
          newnoise <- rlnorm(1,meanlog,sd) 
          noise <- c(noise,newnoise)
          noiseneigh[j] <- newnoise
          neighbors_fitness[j] <- wref*prod(s^mut)*newnoise
        }
      }
 
      position <- which(neighbors_fitness > current_fitness)
      position <- rep(position,2) #because sample function only works with vector of min length 2
      fit[1,1] <- current_fitness
      fit[1,2:(n+1)] <- neighbors_fitness
      k <- 2
      while (length(position) > 0) {
        pos <- sample(position,1)
        gene <- tab[,pos]
        #calculate fitness of each
        current_fitness <- wref*prod(s^gene)*noiseneigh[pos]
        
        #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
        
        neighbors_fitness <- c()
        tab <- matrix(nrow = n, ncol = n)
        noiseneigh <- 1:n
        for (j in 1:n) {
          
          #take genotype i
          mut <- gene
          
          #mutate position j
          mut[j] <- 1 - mut[j] 
          
          #save in column j of matrix tab
          tab[,j] <- mut 
          
          #matrix store all neighbor mutation of Genotype i from genotype_pool
          #each column = 1 neighbor
          posit = which(colSums(mut == genotype_pool) == n)
          if (length(posit) == 1){
            neighbors_fitness[j] <- wref*prod(s^mut)*noise[posit]
            noiseneigh <- noise[posit]
          }else{ #not in the genotype pool yet
            genotype_pool <- cbind(genotype_pool,mut)
            newnoise <- rlnorm(1,meanlog,sd) 
            noise <- c(noise,newnoise)
            noiseneigh <- newnoise
            neighbors_fitness[j] <- wref*prod(s^mut)*newnoise
          }
        }
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
      pathlength[knight,2] <- ratio
      pathlength[knight,3] <- fit[1,1]
      pathlength[knight,4] <- nrow(fit)
      pathlength[knight,5] <- fit[nrow(fit),1]
      pathlength[knight,6] <- numb_fb
      pathlength[knight,7] <- numb_fn
      knight <- knight + 1
    }
    saveRDS(biglist, file = paste("fuji_multi_","n_", as.character(n),"ratio_", as.character(ratio),".rds", sep = ""))
  }
}


saveRDS(pathlength, file = "pathlengths_fuji_multi.rds")

