library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

localoptimalmatrix <- matrix(NA, ncol = 7, nrow = 800000)
colnames(localoptimalmatrix) <- c("genome_size","fn","fd","sample_size","local_optima","k","local_optima_strict")
pawnpower <- 1

for (n in c(5)) {
  for (fd in c(0.5)) {
    for (k in c(1)) {
      #n <- 5 #genome size
      #k <- 1
      m <- 3000 #genotypes sample
      #fd <- 0.2 #lethal fraction
      fn <- 0.3 #neutral fraction
      fb <- 1 - fd -fn #beneficial fraction
      wref <- 2 #reference reproductivity
      
      dummysize <- (2^(k+1))*(2+k)*n*5
      numb_fd <- floor(dummysize*fd)
      numb_fn <- floor(dummysize*fn)
      numb_fb <- dummysize - numb_fn - numb_fd
      b_lambda <- 4*n
      d_lambda <- b_lambda
      dummy <- -rexp(dummysize,d_lambda) #negative exponential distribution
      dummydeleterious <- dummy[which(dummy > -1)] #select only values > -1
      
      #sample genotypes
      genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
      genotype_pool <- as.data.frame(genotype_pool)
      genotype_pool <- genotype_pool %>% distinct() #select only unique rows
      
      
      #calculate fitness of each
      genotype_pool <- t(genotype_pool)
      
      #repeat k position at the end of genes to calculate epistasis
      genotype_pool_withK <- rbind(genotype_pool,genotype_pool[1:k,])
      
      bishop <- 1
      for (bishop in 1:20) {
      # define that fitness is neutral only when itself and all k are off, otherwise not
      s <- array(NA, c(2^(k+1),2+k,n))
      colnames(s) <- c(as.character(1:(k+1)),"fitness_value")
      s[,1:(k+1),] <- rep(as.matrix(expand.grid(rep(list(0:1), k+1))),n) #assign boolean k
      s[1,k+2,] <- rep(0,n) + 1 #assign normal fitness (all other k are 0) 
      dummypool <- c(dummydeleterious[1:numb_fd],
                     rexp(numb_fb,b_lambda),
                     rep(numb_fn,0))
      s[2:(2^(k+1)),k+2,] <- sample(dummypool, size = (n*length(2:(2^(k+1))))) + 1

      #fitness of each position after epistasis
      current_fitness_all_position_after_k <- matrix(nrow = n, ncol = ncol(genotype_pool))
      for (i in 1:n) {
        f_adj_matrix <- s[,,i]
        for (j in 1:ncol(genotype_pool)) {
          current_case <- which(colSums(t(f_adj_matrix[,1:(k+1), drop = F]) == genotype_pool_withK[c((i+1):(i+k),i),j])==(k+1))
          #current_case <- which(apply(as.matrix(f_adj_matrix[,1:(k+1)], ncol=(k+1)), 1, function(x) any(x != genotype_pool_withK[c((i+1):(i+k),i),j]) == F))
          current_fitness_all_position_after_k[i,j] <- f_adj_matrix[current_case,k+2]
        }
      }
      
      current_fitness <- wref*apply(current_fitness_all_position_after_k,2,prod)
      
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
          
        }
        
        #matrix store all neighbor mutation of Genotype i from genotype_pool
        #each column = 1 neighbor
        neighbors[[i]] <- tab 
        
        #repeat k position at the end of genes to calculate epistasis
        tab_withK <- rbind(tab,tab[1:k,])
        
        #fitness of each position after epistasis
        tab_fitness_all_position_after_k <- matrix(nrow = n, ncol = ncol(tab))
        for (a in 1:n) {
          f_adj_matrix <- s[,,a]
          for (b in 1:ncol(tab)) {
            current_case <- which(colSums(t(f_adj_matrix[,1:(k+1),  drop = F]) == tab_withK[c((a+1):(a+k),a),b]) == (k+1))
            #current_case <- which(apply(as.matrix(f_adj_matrix[,1:(k+1)],ncol = (k+1)), 1, function(x) any(x != tab_withK[c((a+1):(a+k),a),b]) == F))
            tab_fitness_all_position_after_k[a,b] <- f_adj_matrix[current_case,k+2]
          }
        }
        
        neighbors_fitness[,i] <- wref*apply(tab_fitness_all_position_after_k,2,prod)
      }
      #(test <- c(n,fn,fd,length(current_fitness), length(which(current_fitness >= apply(neighbors_fitness,2,max))),k, length(which(current_fitness > apply(neighbors_fitness,2,max)))) )
      
      localoptimalmatrix[pawnpower,] <- c(n,fn,fd,length(current_fitness), length(which(current_fitness >= apply(neighbors_fitness,2,max))),k, length(which(current_fitness > apply(neighbors_fitness,2,max)))) 
      pawnpower <- pawnpower + 1
      
      bishop <- bishop + 1
      ######################## plots           
      fitness_neighbor <- as.vector(neighbors_fitness)
      fitness_current <- rep(current_fitness, each = n)
      name <- rep(1:length(current_fitness), each = n)
      fitness_score <- data.frame(fitness_neighbor,fitness_current, name)
      f_cur <- factor(fitness_current)
      #fitness_score$f_current <- as.factor(fitness_score$f_current)
      fitness_score$name <- as.factor(fitness_score$name)
      # Basic violin plot
      # # Basic violin plot
      p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur)) +
        geom_violin() +
        geom_point(size = 0.7)+
        geom_abline(intercept = 0, slope = 1, color = "black") +
        theme(legend.position = "none") +
        xlab("Genotype fitness") + ylab("1-mutant neighbor fitness") 
      p
      
      dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
      colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")
      
      p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Average")
      
      p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        xlim(min(c(current_fitness,neighbors_fitness)),max(current_fitness)) +
        ylim(min(c(current_fitness,neighbors_fitness)),max(neighbors_fitness)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        labs(title = "Maximum")
      
      p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        xlim(min(c(current_fitness,neighbors_fitness)),max(current_fitness)) +
        ylim(min(c(current_fitness,neighbors_fitness)),max(neighbors_fitness)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Minimum")
      
      ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
      #######################  
      }
    }
  }
}


localoptimalmatrix_NK <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_NK, file = "localoptimalmatrix_NK.rds")
#ggpairs(localoptimalmatrix_NK, mapping = NULL, columns = 1:ncol(localoptimalmatrix_NK), title = "NK landscape")
