library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)
n <- 5 #genome size
m <- 1500 #genotypes sample
fd <- 0.2 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
numb_fd <- 2
numb_fn <- 1
numb_fb <- 2
####
n <- 13 #genome size
m <- 100000 #genotypes sample
fd <- 0.2 #lethal fraction
fn <- 0.3 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity
numb_fd <- floor(n*fd)
numb_fn <- floor(n*fn)
numb_fb <- n-numb_fd-numb_fn
b_lambda <- 4*n
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
current_fitness <- wref*apply(s^genotype_pool,2,prod)

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
  neighbors_fitness[,i] <- wref*apply(s^tab,2,prod)
}
#violin
fitness_neighbor <- as.vector(neighbors_fitness)
fitness_current <- rep(current_fitness, each = n)
name <- rep(1:length(current_fitness), each = n)
fitness_score <- data.frame(fitness_neighbor,fitness_current, name)
f_cur <- factor(fitness_current)
#fitness_score$f_current <- as.factor(fitness_score$f_current)
fitness_score$name <- as.factor(fitness_score$name)
library(ggplot2)
# Basic violin plot
p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur)) +
  geom_violin() +
  geom_point(size = 0.1)+
  geom_abline(intercept = 0, slope = 1, color = "black") +
  theme(legend.position = "none") +
  xlab("Genotype fitness") + ylab("1-mutant neighbor fitness") 
  #labs(title=paste("Genome size = ", as.character(n),", advantageous sites = ", as.character(numb_fb),"\n", "neutral sites = ", as.character(numb_fn),", deleterious sites = ", as.character(numb_fd), sep = ""))
  #labs(title=paste("Genome size = ", as.character(n),"\n","Single mutational fitness effects = ", paste(sapply(round(s, digits = 2), as.character), collapse = ", "), sep = ""))
p


#3 plots
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

figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
annotate_figure(figure,
                top = text_grob(paste("Descriptice statistics of 1-mutant neighborhoods' fitness values, genome size = ", as.character(n),"\n","Single mutational fitness effects = ", paste(sapply(round(s, digits = 2), as.character), collapse = ", "), sep = ""), 
                                color = "black", size = 15, hjust = 0, x = 0),
                bottom = text_grob("Genotype fitness"),
                #                    hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Fitness value", rot = 90)
                # right = "I'm done, thanks :-)!",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)
ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),".png",sep = ""),width = 8, height = 3.5)
# #ggexport(figure, filename = paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),".tiff",sep = ""))
# 