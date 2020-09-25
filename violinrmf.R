n <- 5 #genome size
m <- 300 #genotypes sample
fd <- 0.5 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
ratio <- 10
#standarddeviation <-1 #sd of additive fuji

numb_fd <- floor(n*fd)
numb_fn <- floor(n*fn)
numb_fb <- n - numb_fn - numb_fd
b_lambda <- 4*n
d_lambda <- b_lambda
s <- c(-rexp(numb_fd,d_lambda),rep(0,numb_fn),rexp(numb_fb,b_lambda))

#sample genotypes
genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
genotype_pool <- as.data.frame(genotype_pool)
library(tidyverse)
genotype_pool <- genotype_pool %>% distinct() #select only unique rows


#calculate fitness of each
genotype_pool <- t(genotype_pool)
standarddeviation <- 1/b_lambda*ratio

current_fitness <- wref+(apply(s*genotype_pool,2,sum))

# Nonadditive term
noise <- rnorm(length(current_fitness),1,standarddeviation)

current_fitness <- current_fitness + noise

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
    
    # if (length(which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)) == 1){ #1 genotype in the pool is identical to this neighbor
    #   neighbors_fitness[j,i] <- wref+sum(s*mut)+noise[which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)]
    position = which(colSums(mut == genotype_pool) == n)
    if (length(position) == 1){
      neighbors_fitness[j,i] <- wref+sum(s*mut)+noise[position]
    }else{ #not in the genotype pool yet
      genotype_pool <- cbind(genotype_pool,mut)
      newnoise <- rnorm(1,1,standarddeviation)
      noise <- c(noise,newnoise)
      neighbors_fitness[j,i] <- wref+sum(s*mut)+newnoise
    }
  }
  
  #matrix store all neighbor mutation of Genotype i from genotype_pool
  #each column = 1 neighbor
  neighbors[[i]] <- tab 
}

fitness_neighbor <- as.vector(neighbors_fitness)
fitness_current <- rep(current_fitness, each = n)
name <- rep(1:length(current_fitness), each = n)
fitness_score <- data.frame(fitness_neighbor,fitness_current, name)
f_cur <- factor(fitness_current)
fitness_score$f_current <- as.factor(fitness_score$f_current)
fitness_score$name <- as.factor(fitness_score$name)
p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur)) +
  geom_violin() +
  geom_point(size = 0.7)+
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme(legend.position = "none") +
  xlab("Genotype fitness") + ylab("1-mutant neighbor fitness") 
p

