# plot report fig2 ##########
library(ggraph)
library(rgl)
library(igraph)
library(cluster)
library(tidygraph)
n <- 5
genotypes <- as.matrix(expand.grid(rep(list(0:1), n)))
dimin <- 1/8
fitness <- apply(genotypes, 1 , function(x) min(dimin^(sum(x)-1),1)*(0.5^x[1]*1.3^x[4]*2.7^x[5]))
vcol <- c(1:nrow(genotypes))
vframe <- rep("black", nrow(genotypes))
vcol[which(genotypes[,2] == 0 & genotypes[,3] == 0)] <- "skyblue"
vcol[which(genotypes[,2] == 1 & genotypes[,3] == 0)] <- "pink"
vcol[which(genotypes[,2] == 0 & genotypes[,3] == 1)] <- "lightgreen"
vcol[which(genotypes[,2] == 1 & genotypes[,3] == 1)] <- "lightgoldenrod"
vframe[which(genotypes[,1] == 0 & genotypes[,4] == 0 & genotypes[,5] == 0)] <- "red"

veg <-7.5*3
hor <- 2.5*2.5

xaxis <- rep(1,nrow(genotypes))
xaxis[which(genotypes[,2] == 0 & genotypes[,3] == 0)] <- veg
xaxis[which(genotypes[,2] == 1 & genotypes[,3] == 0)] <- 2*veg
xaxis[which(genotypes[,2] == 0 & genotypes[,3] == 1)] <- 3*veg
xaxis[which(genotypes[,2] == 1 & genotypes[,3] == 1)] <- 4*veg

xaxis[which(genotypes[,1] == 1 
            & genotypes[,4] == 1 
            & genotypes[,5] == 0)] <- xaxis[which(genotypes[,1] == 1 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 0)] - hor
xaxis[which(genotypes[,1] == 0 
            & genotypes[,4] == 1 
            & genotypes[,5] == 0)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 1)] - hor

xaxis[which(genotypes[,1] == 0 
            & genotypes[,4] == 0 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 1 
                                                  & genotypes[,5] == 1)] + hor
xaxis[which(genotypes[,1] == 1 
            & genotypes[,4] == 0 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 1 
                                                  & genotypes[,5] == 1)] + hor

nodes <- apply(genotypes, 1, function (x) paste(x,collapse =  ""))
adj_matrix <- daisy(genotypes, metric = "manhattan")
adj_matrix <- as.matrix(adj_matrix)
adj_matrix[which(adj_matrix > 1)] <- 0
colnames(adj_matrix) <- rownames(adj_matrix) <- nodes

g <- graph_from_adjacency_matrix(adj_matrix,mode = "undirected")

coor <- matrix(c(xaxis, fitness),ncol = 2)

tkplot(g,vertex.label.dist=1, vertex.label.cex = 0.7 , vertex.color=vcol, vertex.size = 7,
       vertex.frame.color = vframe, layout = coor )
######## NK ############################
n <- 5 #genome size
k <- 4
m <- 3000 #genotypes sample
fd <- 0.2 #lethal fraction
fn <- 0.4 #neutral fraction
fb <- 1 - fd -fn #beneficial fraction
wref <- 2 #reference reproductivity

dummysize <- (2^(k+1))*(2+k)*n*5
numb_fd <- floor(dummysize*fd)
numb_fn <- floor(dummysize*fn)
numb_fb <- dummysize - numb_fn - numb_fd
b_lambda <- 2
d_lambda <- b_lambda
dummy <- -rexp(dummysize,d_lambda) #negative exponential distribution
dummydeleterious <- dummy[which(dummy > -1)] #select only values > -1

#sample genotypes
genotype_pool <- as.matrix(expand.grid(rep(list(0:1), n)))

#calculate fitness of each
genotype_pool <- t(genotype_pool)

#repeat k position at the end of genes to calculate epistasis
genotype_pool_withK <- rbind(genotype_pool,genotype_pool[1:k,])


  # define that fitness is neutral only when itself and all k are off, otherwise not
  s <- array(NA, c(2^(k+1),2+k,n))
  colnames(s) <- c(as.character(1:(k+1)),"fitness_value")
  s[,1:(k+1),] <- rep(as.matrix(expand.grid(rep(list(0:1), k+1))),n) #assign boolean k
  s[1,k+2,] <- rep(0,n) + 1 #assign normal fitness (all other k are 0) 
  dummypool <- c(dummydeleterious[1:numb_fd],
                 rexp(numb_fb,b_lambda),
                 rep(0,numb_fn))
  s[2:(2^(k+1)),k+2,] <- sample(dummypool, size = (n*length(2:(2^(k+1))))) + 1
  nullcase <- which(apply(s[,1:(k+1),1], 1, function(x) return(all(x == c(rep(0,k),1)))))
  s0 <- s[nullcase,k+2,]
  
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
  current_fitness_null <- wref*apply(s0^genotype_pool,2,prod)


n <- 5
genotypes <- as.matrix(expand.grid(rep(list(0:1), n)))
vcol <- c(1:nrow(genotypes))
vframe <- rep("black", nrow(genotypes))
vcol[which(genotypes[,2] == 0 & genotypes[,3] == 0)] <- "skyblue"
vcol[which(genotypes[,2] == 1 & genotypes[,3] == 0)] <- "pink"
vcol[which(genotypes[,2] == 0 & genotypes[,3] == 1)] <- "lightgreen"
vcol[which(genotypes[,2] == 1 & genotypes[,3] == 1)] <- "lightgoldenrod"
vframe[which(genotypes[,1] == 0 & genotypes[,4] == 0 & genotypes[,5] == 0)] <- "red"

veg <-7.5*3
hor <- 2.5*3.5

xaxis <- rep(1,nrow(genotypes))
xaxis[which(genotypes[,2] == 0 & genotypes[,3] == 0)] <- veg
xaxis[which(genotypes[,2] == 1 & genotypes[,3] == 0)] <- 2*veg
xaxis[which(genotypes[,2] == 0 & genotypes[,3] == 1)] <- 3*veg
xaxis[which(genotypes[,2] == 1 & genotypes[,3] == 1)] <- 4*veg

xaxis[which(genotypes[,1] == 1 
            & genotypes[,4] == 1 
            & genotypes[,5] == 0)] <- xaxis[which(genotypes[,1] == 1 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 0)] - hor
xaxis[which(genotypes[,1] == 0 
            & genotypes[,4] == 1 
            & genotypes[,5] == 0)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 1)] - hor

xaxis[which(genotypes[,1] == 0 
            & genotypes[,4] == 0 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 1 
                                                  & genotypes[,5] == 1)] + hor
xaxis[which(genotypes[,1] == 1 
            & genotypes[,4] == 0 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 1 
                                                  & genotypes[,5] == 1)] + hor
xaxis[which(genotypes[,1] == 0 
            & genotypes[,4] == 1 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 0 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 0)] - 0.5*hor
xaxis[which(genotypes[,1] == 1 
            & genotypes[,4] == 1 
            & genotypes[,5] == 1)] <- xaxis[which(genotypes[,1] == 1 
                                                  & genotypes[,4] == 0 
                                                  & genotypes[,5] == 0)] - 0.5*hor

nodes <- apply(genotypes, 1, function (x) paste(x,collapse =  ""))
adj_matrix <- daisy(genotypes, metric = "manhattan")
adj_matrix <- as.matrix(adj_matrix)
adj_matrix[which(adj_matrix > 1)] <- 0
colnames(adj_matrix) <- rownames(adj_matrix) <- nodes

g <- graph_from_adjacency_matrix(adj_matrix,mode = "undirected")
fitness <- current_fitness
fitness <- current_fitness_null
coor <- matrix(c(xaxis, fitness),ncol = 2)

tkplot(g,vertex.label.dist=1, vertex.label.cex = 0.7 , vertex.size = 7, vertex.color="pink",
        layout = coor )

