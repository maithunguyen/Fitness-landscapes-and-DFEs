for (wref in c(2,5,9)) {
  for (u in c(1e-3,1e-4,1e-6)) {
    n <- 15 #genome size
    refgene <- rep(0,n) #reference genome as a vector of 0s
    #u <- 1e-3 #mutation rate
    fd <- 0.4 #lethal fraction
    fn <- 0.3 #neutral fraction
    fb <- 1 - fd -fn #beneficial fraction
    #wref <- 2 #reference reproductivity
    population_capacity <- 8000
    generations <- 600
    repeat_sim <- 10
    data <- matrix(refgene) 
    s <- c()
    for (i in 1:n) {
      ifelse(i < n*fd, pawn <- runif(1,-2,0),
             ifelse(i < n*(fd+fn), pawn <- 0, pawn <- runif(1,0,2)))
      s <- c(s,pawn)
    } #As a VECTOR assign mutation fitness each position, Wg=wref.Pi(1+si) = 1(1+si)^vi 
    s <- round(s, digits = 2)
    
    ##############################
    mutant_numbers_rep <- list()
    for (l in 1:repeat_sim) {
      
      #Start population
      data <- matrix(refgene, nrow = n) 
      
      #matrix store number of mutations in each position in each generation 
      mutant_number_each_position <- matrix(rep(0,n*generations), nrow = n,ncol = generations) #0 better than NA for the loop of graphs
      #range 0:max() means to NA if it breaks before, so cannot draw the graph
      
      #fraction of mutant in 1 position compare to all mutant in that same generation
      mutant_fraction_each_position <- matrix(rep(0,n*generations), nrow = n,ncol = generations)
      
      #All data on all genes in all generationz
      # store <- list()
      
      for (i in 1:generations){
        #producing children as poisson process with mean calculated from fitness, more realistic than round the fitness
        children_individuals <- sapply(wref+(apply(s*data,2,sum)), function(x) {rpois(1,x)}) #number of children iof each column 
        population_size <- sum(children_individuals)
        if (population_size == 0) break #extince
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
        data <- data1
        mutant_number_each_position[,i] <- rowSums(data)
        mutant_fraction_each_position[,i] <- mutant_number_each_position[,i]/ sum(mutant_number_each_position[,i])
        print(i)
      }
      
      mutant_numbers_rep[[l]] <- mutant_number_each_position
      
      # initiate plot
      plot(range(0:i), range(0:max(mutant_number_each_position, na.rm = T)), type="n", xlab="Generation", ylab="Mutants_number" )
      colors <- rainbow(n) 
      linetype <- c(1:n) 
      
      
      # add lines 
      for (i in 1:n) { 
        lines(mutant_number_each_position[i,], type="l", lwd=2,
              lty=linetype[i], col=colors[i]) 
      } 
      
      # add a title and subtitle 
      title(paste("Mutation number",", Rep = ", as.character(l),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),"\n","u = ", as.character(u),", Wref = ", as.character(wref), sep = ""))
      
      # add a legend 
      legend("bottomright", as.character(c(1:n)), cex= 1, col=rainbow(n) , lty= 1:n, title="position")
      
    }
  }
}
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE) 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="figures_02.2020")
