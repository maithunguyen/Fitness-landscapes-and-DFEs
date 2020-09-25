
########################################################################################################

n <- 19 #genome size
m <- 300 #genotypes sample
refgene <- rep(0,n) #reference genome as a vector of 0s
fd <- 0.4 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
demin <- 0.8 #deminishing return coeff
population_capacity <- 8000
generations <- 600
repeat_sim <- 10
data <- matrix(refgene) 
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

#calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
genotype_pool <- as.matrix(genotype_pool)
neighbors <- list()
neighbors_fitness <- matrix(nrow = n, ncol = ncol(genotype_pool))
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
  neighbors_fitness[,i] <- wref*apply((1+s)^tab,2,prod)
}


# #plot neighbors fitness against current fitness
# 
fitness_neighbor <- as.vector(neighbors_fitness)
fitness_current <- rep(current_fitness, each = n)
fitness_current <- round(fitness_current, digits = 2)
name <- rep(1:length(current_fitness), each = n)
fitness_score <- data.frame(fitness_neighbor,fitness_current, name)
f_cur <- factor(fitness_current)
#fitness_score$f_current <- as.factor(fitness_score$f_current)
fitness_score$name <- as.factor(fitness_score$name)
library(ggplot2)
# Basic violin plot
p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur)) +
  geom_violin() +
  geom_abline(intercept = 0, slope = 1) +
  theme(legend.position = "none") +
  labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
p
# 
# p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor)) +  #, fill = f_cur, color = name)) +  
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1) +
#   theme(legend.position = "none") +
#   labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
# p
# 
# 
# p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur, color = name)) +  
#   scale_fill_viridis_d( option = "D")+
#   #geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black") +
#   geom_boxplot(notch = F,  outlier.size = 2, color="black",lwd=1.2, alpha = 0.7)+
#   geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1) +
#   theme(legend.position = "none") +
#   labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
# p

dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")

p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 
#labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))

p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 
#labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))

p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +  
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme(legend.position = "none") 
#labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))


library(ggpubr)
figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
annotate_figure(figure,
                top = text_grob(paste("Multiplicative landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref), sep = ""), color = "black", face = "bold", size = 14),
                bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
                #                    hjust = 1, x = 1, face = "italic", size = 10),
                # left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                # right = "I'm done, thanks :-)!",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)

