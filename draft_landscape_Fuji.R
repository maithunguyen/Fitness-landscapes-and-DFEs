


####################################################
localoptimalmatrix <- matrix(NA, ncol = 7, nrow = 10000)
colnames(localoptimalmatrix) <- c("genome_size","fd","fb","max_noise","sample_size","local_optima","saddle_points")
pawnpower <- 1
for (n in c(5,6,7,8,9)) {
  for (fd in c(0.1,0.3,0.5,0.6)) {
    for (maxnoisemul in c(1,2,4,8)) {
      #      n <- 6 #genome size
      m <- 300 #genotypes sample
      #      fd <- 0.4 #lethal fraction
      fn <- 0.3 #neutral fraction
      fb <- 1 - fd -fn #beneficial fraction
      wref <- 2 #reference reproductivity
      #     meanpois <- 3 #lambda poisson
      #     maxnoisemul <- 3 #maxfuji
      s <- c()
      for (i in 1:n) {
        ifelse(i < n*fd, pawn <- runif(1,-1,0),
               ifelse(i < n*(fd+fn), pawn <- 0, pawn <- runif(1,0,2)))
        s <- c(s,pawn)
      } #As a VECTOR assign mutation fitness each position, Wg=wref.Pi(1+si) = 1(1+si)^vi 
      s <- round(s, digits = 2)
      
      #sample genotypes
      genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
      genotype_pool <- as.data.frame(genotype_pool)
      #install.packages("tidyverse")
      library(tidyverse)
      genotype_pool <- genotype_pool %>% distinct() #select only unique rows
      
      
      #calculate fitness of each
      genotype_pool <- t(genotype_pool)
      current_fitness <- wref*apply((1+s)^genotype_pool,2,prod)
      
      # Nonadditive term
      #noise <- rnorm(length(current_fitness),1,standarddeviation)
      
      noise <- runif(length(current_fitness),0,maxnoisemul) #uniform disreibution
      
      #noise <- rpois(length(current_fitness),meanpois) #poisson disttribution
      
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
          
          if (length(which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)) == 1){ #1 genotype in the pool is identical to this neighbor
            neighbors_fitness[j,i] <- wref*prod((1+s)^mut)*noise[which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)]
          }else{ #not in the genotype pool yet
            genotype_pool <- cbind(genotype_pool,mut)
            
            newnoise <- runif(1,0,maxnoisemul) #uniform distribution
            
            #newnoise <- rpois(1,meanpois) #poisson distribution
            
            noise <- c(noise,newnoise)
            neighbors_fitness[j,i] <- wref*prod((1+s)^mut)*newnoise
          }
        }
        
        #matrix store all neighbor mutation of Genotype i from genotype_pool
        #each column = 1 neighbor
        neighbors[[i]] <- tab 
        
      }
      
      # dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
      # colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")
      # 
      # p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
      #   geom_point() + 
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none") 
      # #labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
      # 
      # p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
      #   geom_point() + 
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none") 
      # #labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
      # 
      # p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
      #   geom_point() + 
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none") 
      # #labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
      # 
      # figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
      # annotate_figure(figure,
      #                 top = text_grob(paste("Multiplicative Fuji landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref),", max noise", as.character(maxnoisemul), sep = ""), color = "black", face = "bold", size = 14),
      #                 bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
      #                 #                    hjust = 1, x = 1, face = "italic", size = 10),
      #                 # left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
      #                 # right = "I'm done, thanks :-)!",
      #                 #fig.lab = "Figure 1", fig.lab.face = "bold"
      # )
      # ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),"maxnoise", as.character(maxnoisemul),".tiff",sep = ""))
      
      
      localoptimalmatrix[pawnpower,] <- c(n,fd,fb,maxnoisemul,length(current_fitness),length(which(current_fitness > apply(neighbors_fitness,2,max))),length(which(current_fitness == apply(neighbors_fitness,2,max)))) 
      pawnpower <- pawnpower + 1
    }
  }
}

localoptimalmatrix <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])

library(GGally)
ggpairs(localoptimalmatrix, mapping = NULL, columns = 1:ncol(localoptimalmatrix), title = "Fuji multiplicative landscape")




################################################################################################################



n <- 6 #genome size
m <- 300 #genotypes sample
fd <- 0.4 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
standarddeviation <-3 #sd of additive fuji
addgrenze <- 2
meanpois <- 3 #lambda poisson
maxnoisemul <- 3 #maxfuji
s <- c()

for (i in 1:n) {
  ifelse(i < n*fd, pawn <- runif(1,-addgrenze,0),
         ifelse(i < n*(fd+fn), pawn <- 0, pawn <- runif(1,0,addgrenze)))
  s <- c(s,pawn)
} #As a VECTOR assign mutation fitness each position, Wg=wref.Pi(1+si) = 1(1+si)^vi 
s <- round(s, digits = 2)

#sample genotypes
genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
genotype_pool <- as.data.frame(genotype_pool)
#install.packages("tidyverse")
library(tidyverse)
genotype_pool <- genotype_pool %>% distinct() #select only unique rows

##### MULTIPLICATIVE MODEL ##################################


#calculate fitness of each
genotype_pool <- t(genotype_pool)
current_fitness <- wref*apply((1+s)^genotype_pool,2,prod)

# Nonadditive term
#noise <- rnorm(length(current_fitness),1,standarddeviation)

noise <- runif(length(current_fitness),0,maxnoisemul) #uniform disreibution

#noise <- rpois(length(current_fitness),meanpois) #poisson disttribution

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
    
    if (length(which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)) == 1){ #1 genotype in the pool is identical to this neighbor
      neighbors_fitness[j,i] <- wref*prod((1+s)^mut)*noise[which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)]
    }else{ #not in the genotype pool yet
      genotype_pool <- cbind(genotype_pool,mut)
      
      newnoise <- runif(1,0,maxnoisemul) #uniform distribution
      
      #newnoise <- rpois(1,meanpois) #poisson distribution
      
      noise <- c(noise,newnoise)
      neighbors_fitness[j,i] <- wref*prod((1+s)^mut)*newnoise
    }
  }
  
  #matrix store all neighbor mutation of Genotype i from genotype_pool
  #each column = 1 neighbor
  neighbors[[i]] <- tab 
  
}

dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")

p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
annotate_figure(figure,
                top = text_grob(paste("Multiplicative diminishing return landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref),", dimin", as.character(dimin), sep = ""), color = "black", face = "bold", size = 14),
                bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
)
ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),"dimin", as.character(dimin),".tiff",sep = ""))


##### ADDITIVE MODEL ##################################

#calculate fitness of each
genotype_pool <- t(genotype_pool)
current_fitness <- wref+(apply(s*genotype_pool,2,sum))

# Nonadditive term
#noise <- rnorm(length(current_fitness),1,standarddeviation)

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
    
    if (length(which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)) == 1){ #1 genotype in the pool is identical to this neighbor
      neighbors_fitness[j,i] <- wref+sum(s*mut)+noise[which(apply(genotype_pool, 2, function(x) any(x != mut)) == F)]
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

dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")

p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 

figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
annotate_figure(figure,
                top = text_grob(paste("Multiplicative diminishing return landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref),", dimin", as.character(dimin), sep = ""), color = "black", face = "bold", size = 14),
                bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
)
ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),"dimin", as.character(dimin),".tiff",sep = ""))

