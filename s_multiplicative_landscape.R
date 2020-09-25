## Define genome and fitness landscape
n <- 10 #genome size
refgene <- rep(0,n) #reference genome as a vector of 0s
u <- 1e-3 #mutation rate
fl <- 0.3 #lethal fraction
fn <- 0.5 #neutral fraction
fb <- 1 - fl -fn #beneficial fraction
wref <- 2 #reference reproductivity
population_capacity <- 8000
generations <- 300

### DEFINE FITNESS VALUES
# for (i in 1:n) {
#   ifelse(i < n*fl, assign(paste("s",i, sep = "_"), runif(1,-1,0)),
#          ifelse(i < n*(fl+fn), assign(paste("s",i, sep = "_"), 0), 
#                 assign(paste("s",i, sep = "_"), runif(1,0,100))))
#   } #assign mutation fitness each position, Wg=Wref.Pi(1+si) = 1(1+si)^vi
s <- c()
for (i in 1:n) {
  ifelse(i < n*fl, pawn <- runif(1,-1,0),
         ifelse(i < n*(fl+fn), pawn <- 0, pawn <- runif(1,0,2)))
  s <- c(s,pawn)
} #As a VECTOR assign mutation fitness each position, Wg=wref.Pi(1+si) = 1(1+si)^vi 
s <- round(s, digits = 2)
#Start population
data <- matrix(refgene) 

#matrix store number of mutations in each position in each generation 
mutant_number_each_position <- matrix(nrow = n,ncol = generations)

#fraction of mutant in 1 position compare to all mutant in that same generation
mutant_fraction_each_position <- matrix(nrow = n,ncol = generations)

#All data on all genes in all generationz
store <- list()

for (i in 1:generations){
  #producing children as poisson process with mean calculated from fitness, more realistic than round the fitness
  children_individuals <- sapply(wref*(apply((1+s)^data,2,prod)), function(x) {rpois(1,x)}) #number of children iof each column 
  population_size <- sum(children_individuals)
  while(population_size > population_capacity) 
    {
    children_individuals[sample(1:length(children_individuals),50)] <- rep(0,50)
    population_size <- sum(children_individuals)
  }
  data1 <- matrix(nrow = n, ncol = population_size)
  survivals <- which(children_individuals != 0)
  children_individuals <- children_individuals[survivals] #kids of survival only
  data <- matrix(data[,survivals], nrow = n)
    for (j in 1:ncol(data)){
    data1[,(sum(children_individuals[0:(j-1)])+1):
            (sum(children_individuals[0:(j-1)])+children_individuals[j])] <- rep(data[,j],children_individuals[j])
  } #next generation without mutation
  mutationvector <- runif(ncol(data1),0,1)
  mutated_genes_position <- which(mutationvector < u)
  if(length(mutated_genes_position) > 0) for (k in mutated_genes_position) {
           data1[sample(1:n,1),k] <- 1 - data1[sample(1:n,1),k]
         }
  store[[i]] <- data <- data1
  mutant_number_each_position[,i] <- rowSums(store[[i]])
  mutant_fraction_each_position[,i] <- mutant_number_each_position[,i]/ sum(mutant_number_each_position[,i])
  print(i)
}

#saveRDS(store, file = "store.rds") 
#saveRDS(mutant_number_each_position, file = "mutant_number_each position.rds")

# initiate plot
plot(range(0:generations), range(0:max(mutant_number_each_position)), type="n", xlab="Generation", ylab="Mutants_number" )
colors <- rainbow(n) 
linetype <- c(1:n) 


# add lines 
for (i in 1:n) { 
  lines(mutant_number_each_position[i,], type="l", lwd=2,
        lty=linetype[i], col=colors[i]) 
} 

# add a title and subtitle 
title(paste("Mutation number","\n","s = ", paste(sapply(s, as.character), collapse = "_"),"\n","u = ", as.character(u),", Wref = ", as.character(wref), sep = ""))

# add a legend 
legend("bottomright", as.character(c(1:n)), cex= 1, col=rainbow(n) , lty= 1:n, title="position")

#####2nd plott
# initiate plot
plot(range(0:generations), range(0:1), type="n", xlab="Generation", ylab="Mutants_fraction" )
colors <- rainbow(n) 
linetype <- c(1:n) 

# add lines 
for (i in 1:n) { 
  lines(mutant_fraction_each_position[i,], type="l", lwd=2,
        lty=linetype[i], col=colors[i]) 
} 

# add a title and subtitle 
title(paste("Mutation fraction","\n","s = ", paste(sapply(s, as.character), collapse = "_"),"\n","u = ", as.character(u),", Wref = ", as.character(wref), sep = ""))

# add a legend 
legend("topright", as.character(c(1:n)), cex= 1, col=rainbow(n) , lty= 1:n, title="position")

