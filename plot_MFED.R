library(ggplot2)
library(gridExtra)
library(ggpubr)
library(magrittr)
### multi ##################
multi_n100 <- readRDS(file = "multi_n_100.rds")
a <- multi_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
             "neighbor_fitness" = as.vector(a[,2:101]),
             "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha =0.5)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = current_fitness), color = "grey50") +
  labs(x = "Fitness", y = "Scaled density")
  #ggtitle("Neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 100")
p1


# multi_n100 <- readRDS(file = "multi_n_100.rds")
# a <- multi_n100[[1]]
# a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
# data <- data.frame("current_fitness" = rep(a[,1], 100),
#                    "neighbor_fitness" = as.vector(a[,2:101]),
#                    "fill" = as.character(rep(a[,1], 100)))
# p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
#   geom_density(aes(color = fill)) +
#   theme(legend.position = "none") +
#   geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
#   geom_vline(aes(xintercept = current_fitness, color = fill)) +
#   ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 100")
# p2
# 
# ggarrange(p1,p2,ncol = 2, 
#           labels = c("A", "B")                                        # Labels of the scatter plot
# ) 

### dim mul 0.9 adj ##################
dimmulti_n100_d0.9 <- readRDS(file = "dimin_multi_n_100dimin_0.998946949690454.rds")
a <- dimmulti_n100_d0.9[[1]]
aneighbor <- a[,2:ncol(a)]
aminusplus1 <- a[,2:ncol(a)]/a[,1] # normalize the neighbors at 1 = current #
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha =0.5)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = current_fitness), color = "grey50") +
  labs(x = "Fitness", y = "Scaled density")
#ggtitle("Neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 100")
p1

data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))

p2 <- ggplot(data, aes(neighbor_fitness, ..ndensity..)) +
  geom_density(aes(fill = fill, color = fill), alpha =0.07) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Fitness", y = "Scaled density") 
# dimmulti_n100_d0.9 <- readRDS(file = "dimin_multi_n_100dimin_0.998946949690454.rds")
# a <- dimmulti_n100_d0.9[[1]]
# a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
# data <- data.frame("current_fitness" = rep(a[,1], 100),
#                    "neighbor_fitness" = as.vector(a[,2:101]),
#                    "fill" = as.character(rep(a[,1], 100)))
# p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
#   geom_density(aes(color = fill)) +
#   theme(legend.position = "none") +
#   geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
#   geom_vline(aes(xintercept = current_fitness, color = fill)) +
#   ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nDiminishing-returns multiplicative landscape, genome size = 100, dimin = 0.9 adjusted")
# p2

### dim mul 0.5 adj ##################
dimmulti_n100_d0.5 <- readRDS(file = "dimin_multi_n_100dimin_0.993092495437036.rds")
a <- dimmulti_n100_d0.5[[1]]
aneighbor <- a[,2:ncol(a)]
aminusplus1 <- a[,2:ncol(a)]/a[,1] # normalize the neighbors at 1 = current #
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha =0.5)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = current_fitness), color = "grey50") +
  labs(x = "Fitness", y = "Scaled density")
#ggtitle("Neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 100")
ggarrange(p1,p3,ncol = 2,labels = c("A", "B"))

data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))

p4 <- ggplot(data, aes(neighbor_fitness, ..ndensity..)) +
  geom_density(aes(fill = fill, color = fill), alpha =0.07) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Fitness", y = "Scaled density") 
ggarrange(p2,p4,ncol = 2,labels = c("A", "B"))

# dimmulti_n100_d0.5 <- readRDS(file = "dimin_multi_n_100dimin_0.993092495437036.rds")
# a <- dimmulti_n100_d0.5[[1]]
# a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
# data <- data.frame("current_fitness" = rep(a[,1], 100),
#                    "neighbor_fitness" = as.vector(a[,2:101]),
#                    "fill" = as.character(rep(a[,1], 100)))
# p4 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
#   geom_density(aes(color = fill)) +
#   geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
#   theme(legend.position = "none") +
#   geom_vline(aes(xintercept = current_fitness, color = fill)) +
#   ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nDiminishing-returns multiplicative landscape, genome size = 100, dimin = 0.5 adjusted")


### dim add 0.9 adj ##################
dimadd_n100_d0.9 <- readRDS(file = "dimin_add_n_100dimin_0.998946949690454.rds")
a <- dimadd_n100_d0.9[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha = 0.2)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nDiminishing-returns additive landscape, genome size = 100, dimin = 0.9 adjusted")
p1


dimadd_n100_d0.9 <- readRDS(file = "dimin_add_n_100dimin_0.998946949690454.rds")
a <- dimadd_n100_d0.9[[1]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color = fill), alpha = 0) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nDiminishing-returns additive landscape, genome size = 100, dimin = 0.9 adjusted")
p2

### dim add 0.5 adj ##################
dimadd_n100_d0.5 <- readRDS(file = "dimin_add_n_100dimin_0.993092495437036.rds")
a <- dimadd_n100_d0.5[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha = 0.2)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nDiminishing-returns additive landscape, genome size = 100, dimin = 0.5 adjusted")
p1


dimadd_n100_d0.5 <- readRDS(file = "dimin_add_n_100dimin_0.993092495437036.rds")
a <- dimadd_n100_d0.5[[1]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nDiminishing-returns additive landscape, genome size = 100, dimin = 0.5 adjusted")
p2

### fuji add ratio ##################
### 0.01 #####

fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.01.rds")
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji additive landscape, genome size = 100, ratio = 0.01")
p1


fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.01.rds")
a <- fujiadd_n100[[1]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji additive landscape, genome size = 100, ratio = 0.01")
p2

### 0.1 ######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.1.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 0.25 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.25.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 0.4 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.4.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 0.7 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.7.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 1 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_1.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 2 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_2.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### 4 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_4.rds")
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji additive landscape, genome size = 100, ratio = 4")
p1


fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_4.rds")
a <- fujiadd_n100[[5]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji additive landscape, genome size = 100, ratio = 4")
p2
### 10 #######
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_10.rds")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p2
a <- fujiadd_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p1
a <- fujiadd_n100[[7]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
p3
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
### fuji mul ratio adj ##################
### 0.01 #####

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.01.rds")
a <- fujimul_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.01")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.01.rds")
a <- fujimul_n100[[2]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.01")
p2

### 0.1 ######
fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.1.rds")
a <- fujimul_n100[[4]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.1")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.1.rds")
a <- fujimul_n100[[4]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.1")
p2

### 0.25 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.25.rds")
a <- fujimul_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.25")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.25.rds")
a <- fujimul_n100[[1]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.25")
p2

### 0.4 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.4.rds")
a <- fujimul_n100[[5]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.4")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.4.rds")
a <- fujimul_n100[[5]]
#a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.4")
p2

### 0.7 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.7.rds")
a <- fujimul_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.7")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_0.7.rds")
a <- fujimul_n100[[5]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 0.7")
p2

### 1 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_1.rds")
a <- fujimul_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 1")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_1.rds")
a <- fujimul_n100[[5]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 1")
p2


### 2 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_2.rds")
a <- fujimul_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 2")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_2.rds")
a <- fujimul_n100[[5]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 2")
p2


### 4 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_4.rds")
a <- fujimul_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 4")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_4.rds")
a <- fujimul_n100[[5]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 4")
p2
### 10 #######

fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_10.rds")
a <- fujimul_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.05) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 10")
p1


fujimul_n100 <- readRDS(file = "fuji_multi_mf_n_100ratio_10.rds")
a <- fujimul_n100[[3]]
a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nFuji multiplicative landscape, genome size = 100, ratio = 10")
p2

### NK ############
### 20 4 #############
NK <- readRDS(file = "NK_n_20k_4.rds")
n <- 20
k <- 4
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 20 9 #############

NK <- readRDS(file = "NK_n_20k_9.rds")
n <- 20
k <- 9
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 20 14 #############

NK <- readRDS(file = "NK_n_20k_14.rds")
n <- 20
k <- 14
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 50 5 #############

NK <- readRDS(file = "NK_n_50k_5.rds")
n <- 50
k <- 5
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 50 10 #############

NK <- readRDS(file = "NK_n_50k_10.rds")
n <- 50
k <- 10
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 6 1 #############
NK <- readRDS(file = "NK_n_6k_1.rds")
n <- 6
k <- 1
plotlist <- list()
i <- 1

for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
    plotlist[[i]] <- p
    i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 6 2 #############
NK <- readRDS(file = "NK_n_6k_2.rds")
n <- 6
k <- 2
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 6 4 #############
NK <- readRDS(file = "NK_n_6k_4.rds")
n <- 6
k <- 4
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 6 5 #############
NK <- readRDS(file = "NK_n_6k_5.rds")
n <- 6
k <- 5
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 8 2 #############
NK <- readRDS(file = "NK_n_8k_2.rds")
n <- 8
k <- 2
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 8 3 #############
NK <- readRDS(file = "NK_n_8k_3.rds")
n <- 8
k <- 3
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 8 5 #############
NK <- readRDS(file = "NK_n_8k_5.rds")
n <- 8
k <- 5
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 8 6 #############
NK <- readRDS(file = "NK_n_8k_6.rds")
n <- 8
k <- 6
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 10 2 #############
NK <- readRDS(file = "NK_n_10k_2.rds")
n <- 10
k <- 2
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 10 4 #############
NK <- readRDS(file = "NK_n_10k_4.rds")
n <- 10
k <- 4
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 10 6 #############
NK <- readRDS(file = "NK_n_10k_6.rds")
n <- 10
k <- 6
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 10 8 #############
NK <- readRDS(file = "NK_n_10k_8.rds")
n <- 10
k <- 8
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 12 2 #############
NK <- readRDS(file = "NK_n_12k_2.rds")
n <- 12
k <- 2
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 12 5 #############
NK <- readRDS(file = "NK_n_12k_5.rds")
n <- 12
k <- 5
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 12 7 #############
NK <- readRDS(file = "NK_n_12k_7.rds")
n <- 12
k <- 7
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 12 10 #############
NK <- readRDS(file = "NK_n_12k_10.rds")
n <- 12
k <- 10
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 14 3 #############
NK <- readRDS(file = "NK_n_14k_3.rds")
n <- 14
k <- 3
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 14 6 #############
NK <- readRDS(file = "NK_n_14k_6.rds")
n <- 14
k <- 6
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 14 8 #############
NK <- readRDS(file = "NK_n_14k_8.rds")
n <- 14
k <- 8
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 14 11 #############
NK <- readRDS(file = "NK_n_14k_11.rds")
n <- 14
k <- 11
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 25 2 #############
NK <- readRDS(file = "NK_n_25k_2.rds")
n <- 25
k <- 2
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 35 4 #############
NK <- readRDS(file = "NK_n_35k_4.rds")
n <- 35
k <- 4
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 45 4 #############
NK <- readRDS(file = "NK_n_45k_4.rds")
n <- 45
k <- 4
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")

### 20 8 #############
NK <- readRDS(file = "NK_n_20k_8.rds")
n <- 20
k <- 8
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")


### 20 12 #############
NK <- readRDS(file = "NK_n_20k_12.rds")
n <- 20
k <- 12
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")


### 20 16 #############
NK <- readRDS(file = "NK_n_20k_16.rds")
n <- 20
k <- 16
plotlist <- list()
i <- 1
for (i in 1:9) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    theme(axis.title = element_blank())
  plotlist[[i]] <- p
  i <- i + 1
}

main <- grid.arrange(grobs = plotlist, 
                     ncol = 3, 
                     top = paste("NK landscape, genome size = ", as.character(n),", k = ",as.character(k),sep = ""),
                     bottom = "neighbor fitness",
                     left = "density scaled")
### 100 ##################
NK <- readRDS(file = "NK_n_100k_80.rds")
n <- 100
k <- 80
plotlist <- list()
i <- 1
for (i in 1:2) {
  a <- NK[[i]]
  data <- data.frame("current_fitness" = rep(a[,1], n),
                     "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                     "fill" = as.character(rep(a[,1], n)))
  p <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
    geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
    theme(legend.position = "none") +
    geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
    geom_vline(aes(xintercept = current_fitness, color = fill)) +
    labs(x = "Fitness", y = "Scaled density")
  plotlist[[i]] <- p
  i <- i + 1
}
p1 <- plotlist[[1]]
p2 <- plotlist[[2]]
grid.arrange(p1,p2,ncol = 2)




a <- readRDS(file = "pathlengths_fuji_add_3.rds")
