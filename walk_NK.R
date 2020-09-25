library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

numb_start <- 10
npool <- c(20,40,60,80,100)
kpool <- seq(4,20,5)
pathlength <- matrix(ncol = 7, nrow = numb_start*length(npool)*length(kpool))
colnames(pathlength) <- c("n","k","fstart","steps","fend","fb","fn")
knight <- 1
for (n in npool) {
  for (k in kpool) {
    #n <- 5 #genome size
    ka <- 1
    m <- 100 #genotypes sample
    fd <- 0.5 #lethal fraction
    fn <- 0.3 #neutral fraction
    fb <- 1 - fd -fn #beneficial fraction
    wref <- 2 #reference reproductivity
    dummysize <- (2^(k+1))*(2+k)*n*5
    numb_fd <- floor(dummysize*fd)
    numb_fn <- floor(dummysize*fn)
    numb_fb <- dummysize - numb_fn - numb_fd
    b_lambda <- 4
    d_lambda <- b_lambda
    dummy <- -rexp(dummysize,d_lambda) #negative exponential distribution
    dummydeleterious <- dummy[which(dummy > -1)] #select only values > -1
    
    # define that fitness is neutral only when itself and all k are off, otherwise not
    s <- array(NA, c(2^(k+1),2+k,n))
    colnames(s) <- c(as.character(1:(k+1)),"fitness_value")
    s[,1:(k+1),] <- rep(as.matrix(expand.grid(rep(list(0:1), k+1))),n) #assign boolean k + 1
    s[1,k+2,] <- rep(0,n) + 1 #assign normal fitness (all other k are 0) 
    dummypool <- c(dummydeleterious[1:numb_fd],
                   rexp(numb_fb,b_lambda),
                   rep(numb_fn,0))
    s[2:(2^(k+1)),k+2,] <- sample(dummypool, size = (n*length(2:(2^(k+1))))) + 1
    s <- round(s, digits = 2)
    
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
      gene_w_K <- c(gene, gene[1:k])
      current_fitness_all_position_after_k <- c()
      for (i in 1:n) {
        f_adj_matrix <- s[,,i]
        current_case <- which(colSums(t(f_adj_matrix[,1:(k+1)]) == gene_w_K[c((i+1):(i+k),i)])==(k+1))
        current_fitness_all_position_after_k[i] <- f_adj_matrix[current_case,k+2]
      }
      current_fitness <- wref*prod(current_fitness_all_position_after_k)
      
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
      
      #repeat k position at the end of genes to calculate epistasis
      tab_withK <- rbind(tab,tab[1:k,])
      
      #fitness of each position after epistasis
      tab_fitness_all_position_after_k <- matrix(nrow = n, ncol = ncol(tab))
      for (a in 1:n) {
        f_adj_matrix <- s[,,a]
        for (b in 1:ncol(tab)) {
          current_case <- which(colSums(t(f_adj_matrix[,1:(k+1)]) == tab_withK[c((a+1):(a+k),a),b]) == (k+1))
          tab_fitness_all_position_after_k[a,b] <- f_adj_matrix[current_case,k+2]
        }
      }
      
      neighbors_fitness <- wref*apply(tab_fitness_all_position_after_k,2,prod)
      
      position <- which(neighbors_fitness > current_fitness)
      position <- rep(position,2) #because sample function only works with vector of min length 2
      
      fit[1,1] <- current_fitness
      fit[1,2:(n+1)] <- neighbors_fitness
      ka <- 2
      while (length(position) > 0) {
        gene <- tab[,sample(position,1)]
        #calculate fitness of each
        gene_w_K <- c(gene, gene[1:k])
        current_fitness_all_position_after_k <- c()
        for (i in 1:n) {
          f_adj_matrix <- s[,,i]
          current_case <- which(colSums(t(f_adj_matrix[,1:(k+1)]) == gene_w_K[c((i+1):(i+k),i)])==(k+1))
          current_fitness_all_position_after_k[i] <- f_adj_matrix[current_case,k+2]
        }
        current_fitness <- wref*prod(current_fitness_all_position_after_k)
        
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
        
        #repeat k position at the end of genes to calculate epistasis
        tab_withK <- rbind(tab,tab[1:k,])
        
        #fitness of each position after epistasis
        tab_fitness_all_position_after_k <- matrix(nrow = n, ncol = ncol(tab))
        for (a in 1:n) {
          f_adj_matrix <- s[,,a]
          for (b in 1:ncol(tab)) {
            current_case <- which(colSums(t(f_adj_matrix[,1:(k+1)]) == tab_withK[c((a+1):(a+k),a),b]) == (k+1))
            tab_fitness_all_position_after_k[a,b] <- f_adj_matrix[current_case,k+2]
          }
        }
        
        neighbors_fitness <- wref*apply(tab_fitness_all_position_after_k,2,prod)
        
        position <- which(neighbors_fitness > current_fitness)
        position <- rep(position,2)
        fit[ka,1] <- current_fitness
        fit[ka,2:(n+1)] <- neighbors_fitness
        ka <- ka+1
      }
      fit <- fit[1:(ka-1),,drop = F]
      biglist[[id]] <- fit
      id <- id + 1
      pathlength[knight,1] <- n
      pathlength[knight,2] <- k
      pathlength[knight,3] <- fit[1,1]
      pathlength[knight,4] <- nrow(fit)
      pathlength[knight,5] <- fit[nrow(fit),1]
      pathlength[knight,6] <- fb
      pathlength[knight,7] <- fn
      knight <- knight + 1
    }
    saveRDS(biglist, file = paste("NK_","n_", as.character(n),"k_", as.character(k),".rds", sep = ""))
  }
}


saveRDS(pathlength, file = "pathlengths_NK_multi.rds")

