library(ggplot2)
library(magrittr)
library(gridExtra)
library(ggpubr)
library(MASS)
library(fitdistrplus)
### multi, fn = 0.3 ##################
multi_n100 <- readRDS(file = "multi_n_100.rds")
a <- multi_n100[[9]]
aneighbor <- a[,2:101]
### normalize the neighbors at 1 = current #####
aminusplus1 <- a[,2:101]/a[,1]
### fit distribution of normalized data #######
storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 3)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(aminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
}

storedata <- data.frame(rep(storedistpara[,1],2),
  c(storedistpara[,2],storedistpara[,3]),
  c(rep("shape",nrow(storedistpara)),rep("rate",nrow(storedistpara))))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 3, alpha= 0.6) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 100, fn = 0.3, fb = 0.5")
p

### Plot normalized data distributions #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha =0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("MFEDs during an adaptive walk, normalized with current fitness = 1\nMultiplicative landscape, genome size = 100, fn = 0.3, fb = 0.5")
p1

#######################
### omit neutral sites ########
vaneighbor <- matrix(aneighbor[-which(aminus == 0)],nrow = nrow(aminus))
current_fitness <- a[,1]
### plot MFEDs omit ######
data <- data.frame("current_fitness" = rep(a[,1], ncol(vaneighbor)),
                   "neighbor_fitness" = as.vector(vaneighbor),
                   "fill" = as.character(rep(a[,1], ncol(vaneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha =0.02)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 70 omit neutral sites")
p1



v <- as.vector(aminus)
v <- v[ v != 0 ]
vaminus <- matrix(v,nrow = nrow(aminus)) # normalized around 0

vaminusplus1 <- vaminus + 1 # normalize at 1
mean <- rowMeans(vaminus)
std = apply(vaminus, 1, sd)
plot(mean)
plot(std)

### fit distribution ####

storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 3)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(vaminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
}

storedata <- data.frame(rep(storedistpara[,1],2),
                        c(storedistpara[,2],storedistpara[,3]),
                        c(rep("shape",nrow(storedistpara)),rep("rate",nrow(storedistpara))))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 3, alpha= 0.6) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 70 omitted neutral sites")
p
fit.gamma <- fitdist(vaminusplus1[41,], distr = "gamma", method = "mle")
summary(fit.gamma)
plot(fit.gamma)

### plot normalized distribution #######
data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(vaminusplus1),
                   "fill" = as.character(rep(a[,1], 70)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha =0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Normalized MFEDs during an adaptive walk\nMultiplicative landscape, genome size = 70, omit neutral sites")
p1

#######################
### new data wo neutral in simulation #######

multi_n70 <- readRDS(file = "multi_n_70.rds")
a <- multi_n70[[2]]
aneighbor <- a[,-1]
mean <- rowMeans(aneighbor)
std = apply(a[,-1], 1, sd)
plot(mean)
plot(std)

### plot MFED ########
data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(a[,-1]),
                   "fill" = as.character(rep(a[,1], 70)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha =0.2) +
  geom_vline(aes(xintercept = current_fitness)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 70, fn=0,fb=5/7")
p1

a <- a[c(1,seq(5,nrow(a),5),nrow(a)),]
data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(a[,-1]),
                   "fill" = as.character(rep(a[,1], 70)))

p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill)) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  ggtitle("Genotype's fitness and its neighborhood fitness density during an adaptive walk\nMultiplicative landscape, genome size = 100")
p2

### normalize neighbors at 1 ########
aminusplus1 <- a[,-1]/a[,1]
#aminusplus1 <- aminus + 1
mean <- rowMeans(aminus)
std = apply(aminus, 1, sd)
plot(mean)
plot(std)

data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 70)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha =0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Normalized MFEDs during an adaptive walk\nMultiplicative landscape, genome size = 70, fn = 0, fb = 5/7")
p1

### fit distribution on normalized data ###########
storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 3)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(aminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
}

storedata <- data.frame(rep(storedistpara[,1],2),
                        c(storedistpara[,2],storedistpara[,3]),
                        c(rep("shape",nrow(storedistpara)),rep("rate",nrow(storedistpara))))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 3, alpha= 0.6) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 70, fn = 0, fb = 5/7")
p
### separate down up hill ###############
uphill <- rowSums(ifelse(aminus>0, 1, 0))
downhill <- rowSums(ifelse(aminus<0, 1, 0))
neutral <- rowSums(ifelse(aminus==0, 1, 0))
plot(uphill)
plot(downhill)
plot(neutral)
plot(uphill/downhill)
plot(downhill/uphill)

###############################
###############################
### dimin mul 0.5 ###############
dimmulti_n100_d0.5 <- readRDS(file = "dimin_multi_n_100dimin_0.993092495437036.rds")
a <- dimmulti_n100_d0.5[[1]]
aneighbor <- a[,-1]

### normalize neighbors at 1 ########
aminusplus1 <- a[,-1]/a[,1]
#aminusplus1 <- aminus + 1

data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("Normalized MFEDs during an adaptive walk\nDiminishing returns multiplicative landscape, genome size = 100, dim = 0.5, fn = 0.3, fb = 0.5")
p1

### fit bimodel distribution on normalized data ###########
### gamma distribution ######
library(mixdist)
it <- as.matrix(table(aminusplus1[4,]))
it2 <- sort(unique(aminusplus1[4,]),decreasing = F)
it <- matrix(c(it2,it),ncol = 2)
it <- as.data.frame(it)
it <- as.mixdata(it)
dt <- aminusplus1[4,]
mu <- c(mean(dt[which(dt < 1)]),mean(dt[which(dt > 1)]))
sigma <- c(sd(dt[which(dt < 1)]),sd(dt[which(dt > 1)]))
lambda <- c(length(which(dt < 1)),length(which(dt > 1)))/length(dt)

param <- mixparam(mu = mu, sigma = sigma, lambda)
fit.gamma <- mix(it,param,"gamma")
summary(fit.gamma)
plot(fit.gamma)

### normal distribution ######

library(mixtools)

storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 7)
for (i in 1:nrow(aminusplus1)) {
  dt <- aminusplus1[i,]
  mu <- c(mean(dt[which(dt < 1)]),mean(dt[which(dt > 1)]))
  sigma <- c(sd(dt[which(dt < 1)]),sd(dt[which(dt > 1)]))
  lambda <- c(length(which(dt < 1)),length(which(dt > 1)))/length(dt)
  fit.gamma <- normalmixEM(aminusplus1[i,], lambda = lambda, mu = mu, sigma = sigma)
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$lambda[1]
  storedistpara[i,3] <- fit.gamma$lambda[2]
  storedistpara[i,4] <- fit.gamma$mu[1]
  storedistpara[i,5] <- fit.gamma$mu[2]
  storedistpara[i,6] <- fit.gamma$sigma[1]
  storedistpara[i,7] <- fit.gamma$sigma[2]
}

storedata <- data.frame(rep(storedistpara[,1],10),
                        c(storedistpara[,2],
                          storedistpara[,3],
                          storedistpara[,4],
                          storedistpara[,5],
                          storedistpara[,6],
                          storedistpara[,7],
                          (storedistpara[,4]/storedistpara[,6])^2,
                          (storedistpara[,5]/storedistpara[,7])^2,
                          storedistpara[,4]/(storedistpara[,6]^2),
                          storedistpara[,5]/(storedistpara[,7]^2)),
                        c(rep("lambda1",nrow(storedistpara)),
                          rep("lambda2",nrow(storedistpara)),
                          rep("mu1",nrow(storedistpara)),
                          rep("mu2",nrow(storedistpara)),
                          rep("sigma1",nrow(storedistpara)),
                          rep("sigma2",nrow(storedistpara)),
                          rep("shape1",nrow(storedistpara)),
                          rep("shape2",nrow(storedistpara)),
                          rep("rate1",nrow(storedistpara)),
                          rep("rate2",nrow(storedistpara))
                          ))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata[(nrow(storedistpara)*6+1):nrow(storedata),]) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 2, alpha= 0.7) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nDiminishing returns multiplicative landscape, genome size = 100, dim = 0.5, fn = 0.3, fb = 0.5")
p
p <- ggplot(storedata[1:nrow(storedistpara)*6,]) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 2, alpha= 0.7) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nDiminishing returns multiplicative landscape, genome size = 100, dim = 0.5, fn = 0.3, fb = 0.5")
p
###############################
###############################
### fuji add 0.4 ###############
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_0.4.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]

### normalize neighbors at 1 ########
aminusplus1 <- a[,-1]/a[,1]
#aminusplus1 <- aminus + 1

data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1, color = "red")) +
  theme(legend.position = "none") +
  ggtitle("Normalized MFEDs during an adaptive walk\nRMF landscape, theta = 0.4, genome size = 100, fn = 0.3, fb = 0.2")
p1

### fit gamma distribution on normalized data ###########
storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 3)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(aminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
}

storedata <- data.frame(rep(storedistpara[,1],2),
                        c(storedistpara[,2],storedistpara[,3]),
                        c(rep("shape",nrow(storedistpara)),rep("rate",nrow(storedistpara))))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 3, alpha= 0.6) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nRMF landscape, theta = 0.4, genome size = 100, fn = 0.3, fb = 0.2")
p

###############################
###############################
### NK ###############
NK <- readRDS(file = "NK_n_14k_3.rds")
a <- NK[[8]]
aneighbor <- a[,-1]
mean <- rowMeans(aneighbor)
std = apply(aneighbor, 1, sd)
plot(mean)
plot(std)
### normalize neighbors at 1 ########
aminusplus1 <- a[,-1]/a[,1]
#aminusplus1 <- aminus + 1

data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1, color = "red")) +
  theme(legend.position = "none") +
  ggtitle("Normalized MFEDs during an adaptive walk\nNK landscape, genome size = 14, k = 8, fn = 0.3, fb = 0.2")
p1

### fit gamma distribution on normalized data ###########
storedistpara <- matrix(nrow = nrow(aminusplus1), ncol = 3)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(aminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
}

storedata <- data.frame(rep(storedistpara[,1],2),
                        c(storedistpara[,2],storedistpara[,3]),
                        c(rep("shape",nrow(storedistpara)),rep("rate",nrow(storedistpara))))
colnames(storedata) <- c("step","unit","parameter")
p <- ggplot(storedata) +
  geom_point(aes(step, unit, shape = parameter, color = parameter), size = 3, alpha= 0.6) +
  ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nNK landscape, genome size = 14, k = 8, fn = 0.3, fb = 0.2")
p
