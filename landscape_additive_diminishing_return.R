library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

localoptimalmatrix <- matrix(NA, ncol = 11, nrow = 10000)
colnames(localoptimalmatrix) <- c("genome_size","numb_fn","numb_fb","numb_fd",
                                  "fn","fb","fd","dimin","sample_size","local_optima","local_optima_strict")
pawnpower <- 1

for (n in c(5,7,9,10,11,13)) {
  for (fd in c(0.2,0.3,0.4,0.5,0.6)) {
    #n <- 5 #genome size
    m <- 70000 #genotypes sample
    #fd <- 0.5 #lethal fraction
    fn <- 0.3 #neutral fraction
    fb <- 1 - fd -fn #beneficial fraction
    wref <- 2 #reference reproductivity
    numb_fd <- round(n*fd)
    numb_fn <- round(n*fn)
    numb_fb <- n - numb_fn - numb_fd
    b_lambda <- 4
    d_lambda <- b_lambda
    s <- c(-rexp(numb_fd,d_lambda),rep(0,numb_fn),rexp(numb_fb,b_lambda))
    s <- round(s, digits = 2)
    
    #sample genotypes
    genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
    genotype_pool <- as.data.frame(genotype_pool)
    #install.packages("tidyverse")
    library(tidyverse)
    genotype_pool <- genotype_pool %>% distinct() #select only unique rows
    
    #calculate fitness of each
    genotype_pool <- t(genotype_pool)
    
    #calculate fitness of neighbors (1 mutation away for the chosen genotype), 1 genotype has n neighbors
    genotype_pool <- as.matrix(genotype_pool)
    neighbors <- list()
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
    }
    
    for (dimin in c(seq(0,1,0.05))) {
      
      number_of_mutation <- colSums(genotype_pool)
      individuals_with_multiple_mutation <- which(number_of_mutation > 1)
      children_individuals <- wref+(apply(s*genotype_pool,2,sum))
      children_individuals[individuals_with_multiple_mutation] <- (dimin^(number_of_mutation[individuals_with_multiple_mutation]-1))*children_individuals[individuals_with_multiple_mutation]
      
      current_fitness <- children_individuals
      
      neighbors_fitness <- matrix(nrow = n, ncol = ncol(genotype_pool))
      for (f in 1:ncol(genotype_pool)) {
        number_of_mutation <- colSums(neighbors[[f]])
        individuals_with_multiple_mutation <- which(number_of_mutation > 1)
        children_individuals <- wref+(apply(s*neighbors[[f]],2,sum))
        children_individuals[individuals_with_multiple_mutation] <- (dimin^(number_of_mutation[individuals_with_multiple_mutation]-1))*children_individuals[individuals_with_multiple_mutation]
        
        neighbors_fitness[,f] <- children_individuals
      }
      
      # fitness_neighbor <- as.vector(neighbors_fitness)
      # fitness_current <- rep(current_fitness, each = n)
      # name <- rep(1:length(current_fitness), each = n)
      # fitness_score <- data.frame(fitness_neighbor,fitness_current, name)
      # f_cur <- factor(fitness_current)
      # #fitness_score$f_current <- as.factor(fitness_score$f_current)
      # fitness_score$name <- as.factor(fitness_score$name)
      # library(ggplot2)
      # # Basic violin plot
      # p <- ggplot(fitness_score, aes(x=fitness_current, y=fitness_neighbor, fill = f_cur)) +
      #   geom_point() +
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none") +
      #   labs(title=paste("Diminishing-returns additive landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", dimin = ", as.character(dimin),", Wref = ", as.character(wref), sep = ""))
      # p
      # 
      # dataplot <- data.frame(current_fitness,colSums(neighbors_fitness)/n,apply(neighbors_fitness,2,max),apply(neighbors_fitness,2,min))
      # colnames(dataplot) <- c("current_fitness","average_neighbor_fitness","max_neighbor_fitness","min_neighbor_fitness")
      # 
      # p1 <- ggplot(dataplot, aes(x=current_fitness, y=average_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
      #   geom_point() +
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none")
      # 
      # p2 <- ggplot(dataplot, aes(x=current_fitness, y=max_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
      #   geom_point() +
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none")
      # 
      # p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
      #   geom_point() +
      #   geom_abline(intercept = 0, slope = 1, color = "red") +
      #   theme(legend.position = "none")
      # 
      # figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
      # annotate_figure(figure,
      #                 top = text_grob(paste("Additive diminishing return landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref), sep = ""), color = "black", face = "bold", size = 14),
      #                 bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
      # )
      # ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),".tiff",sep = ""))
      # 
      localoptimalmatrix[pawnpower,] <- c(n,
                                          length(which(s == 0)),
                                          length(which(s > 0)),
                                          length(which(s < 0)),
                                          fn,fb,fd,dimin,
                                          length(current_fitness),
                                          length(which(current_fitness >= apply(neighbors_fitness,2,max))),
                                          length(which(current_fitness > apply(neighbors_fitness,2,max)))) 
      pawnpower <- pawnpower + 1
    }
  }
}

localoptimalmatrix_add_return <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_add_return, file = "localoptimalmatrix_add_return.rds")
#ggpairs(localoptimalmatrix_add_return, mapping = NULL, columns = 1:ncol(localoptimalmatrix_add_return), title = "diminishing return additive landscape")
