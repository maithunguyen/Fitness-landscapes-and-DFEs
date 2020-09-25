library(ggplot2)
library(ggpubr)
library(GGally)
library(tidyverse)

localoptimalmatrix <- matrix(NA, ncol = 6, nrow = 800)
colnames(localoptimalmatrix) <- c("genome_size","fd","fb","sample_size","local_optima","saddle_points")
pawnpower <- 1

for (n in c(5,7,9,13)) {
  for (fd in c(0.2,0.4,0.6)) {
      #n <- 5 #genome size
      m <- 1500 #genotypes sample
      #fd <- 0.2 #lethal fraction
      fn <- 0.3 #neutral fraction
      fb <- 1 - fd -fn #beneficial fraction
      wref <- 2 #reference reproductivity
      numb_fd <- floor(n*fd)
      numb_fn <- floor(n*fn)
      numb_fb <- n - numb_fn - numb_fd
      b_lambda <- 4
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
        xlim(min(c(current_fitness,neighbors_fitness)),max(current_fitness)) +
        ylim(min(c(current_fitness,neighbors_fitness)),max(neighbors_fitness)) +
        theme(legend.position = "none")
      #labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))
      
      p3 <- ggplot(dataplot, aes(x=current_fitness, y=min_neighbor_fitness,fill = factor(current_fitness))) +  #, fill = f_cur, color = name)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        xlim(min(c(current_fitness,neighbors_fitness)),max(current_fitness)) +
        ylim(min(c(current_fitness,neighbors_fitness)),max(neighbors_fitness)) +
        theme(legend.position = "none")
      #labs(title=paste("Multiplicative landscape",", genome size = ", as.character(n),"\n","s = ", paste(sapply(s, as.character), collapse = "_"),", Wref = ", as.character(wref), sep = ""))

      figure <- ggarrange(p1,p2,p3,ncol = 3, nrow = 1)
      annotate_figure(figure,
                      top = text_grob(paste("Multiplicative landscape",", genome size = ", as.character(n),", genotypes sample = ", as.character(length(current_fitness)),"\n","fb = ", as.character(fb),", fd = ", as.character(fd),", Wref = ", as.character(wref), sep = ""), color = "black", face = "bold", size = 14),
                      bottom = text_grob(paste("s = ", paste(sapply(s, as.character), collapse = "_")), color = "blue")
                      #                    hjust = 1, x = 1, face = "italic", size = 10),
                      # left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                      # right = "I'm done, thanks :-)!",
                      #fig.lab = "Figure 1", fig.lab.face = "bold"
      )
       ggsave(filename=paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),".png",sep = ""),width = 8, height = 3.5)
      # #ggexport(figure, filename = paste("n", as.character(n),"m", as.character(length(current_fitness)),"fb", as.character(fb),"fd", as.character(fd),".tiff",sep = ""))
      # 
      localoptimalmatrix[pawnpower,] <- c(n,fd,fb,length(current_fitness),length(which(current_fitness > apply(neighbors_fitness,2,max))),length(which(current_fitness == apply(neighbors_fitness,2,max)))) 
      pawnpower <- pawnpower + 1
    }
  }

localoptimalmatrix_multi <- as.data.frame(localoptimalmatrix[1:(pawnpower-1),])
saveRDS(localoptimalmatrix_multi, file = "localoptimalmatrix_multi.rds")
ggpairs(localoptimalmatrix_multi, mapping = NULL, columns = 1:ncol(localoptimalmatrix_multi), title = "Multiplicative landscape")


