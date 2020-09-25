localoptimalmatrix <- matrix(NA, ncol = 8, nrow = 10000)
colnames(localoptimalmatrix) <- c("genome_size","numb_fn","numb_fb","sd_noise","ratio_mean_to_sdnoise","sample_size","local_optima","local_optima_strict")
pawnpower <- 1

for (n in c(5,7,9,10,11,13)) {
    #n <- 5 #genome size
    m <- 70000 #genotypes sample
    fd <- 0.2 #lethal fraction
    fn <- 0.3 #neutral fraction
    fb <- 1 - fd -fn #beneficial fraction
    wref <- 2 #reference reproductivity
    #standarddeviation <-1 #sd of additive fuji
    
    numb_fd <- floor(n*fd)
    numb_fn <- floor(n*fn)
    numb_fb <- n - numb_fn - numb_fd
    b_lambda <- 4
    d_lambda <- b_lambda
    s <- c(-rexp(numb_fd,d_lambda),rep(0,numb_fn),rexp(numb_fb,b_lambda))
    
    #sample genotypes
    genotype_pool <- matrix(sample(0:1, n * m, replace = TRUE), m, n)
    genotype_pool <- as.data.frame(genotype_pool)
    #install.packages("tidyverse")
    library(tidyverse)
    genotype_pool <- genotype_pool %>% distinct() #select only unique rows
    
    
    #calculate fitness of each
    genotype_pool <- t(genotype_pool)
    
    for (ratio in c(0.0001,0.001,seq(0.01,1.9,0.06))) {
      standarddeviation <- 1/b_lambda/ratio
      bishop <- 1
      for (bishop in 1:100) {
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
      #   labs(title=paste("Fuji additive landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref),", sd = ", as.character(standarddeviation), sep = ""))
      # p
      # 
      # 
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
      #                 top = text_grob(paste("Fuji additive landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref),", sd = ", as.character(standarddeviation),", b_lambda = ", as.character(b_lambda), sep = ""), color = "black", face = "bold", size = 14),
      #                 bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
      # )
      # ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),"sd", as.character(standarddeviation),"b_lambda", as.character(b_lambda),".tiff",sep = ""))
      
      localoptimalmatrix[pawnpower,] <- c(n,
                                          length(which(s == 0)),
                                          length(which(s > 0)),
                                          standarddeviation,
                                          ratio,
                                          length(current_fitness),
                                          length(which(current_fitness >= apply(neighbors_fitness,2,max))),
                                          length(which(current_fitness > apply(neighbors_fitness,2,max)))) 
      pawnpower <- pawnpower + 1
      bishop <- bishop + 1
      }
    }
  }


localoptimalmatrix_fuji_additive <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_fuji_additive, file = "localoptimalmatrix_fuji_additive_ratio.rds")

