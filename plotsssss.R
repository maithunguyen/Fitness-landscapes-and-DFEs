library(ggplot2)
library(magrittr)
library(gridExtra)
library(ggpubr)
library(MASS)
library(fitdistrplus)
library(cowplot)
library(grid)
#library(tikzDevice)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#library(timeDate)
#library(timeSeries)
#library(fBasics)
library(moments)
#library(PerformanceAnalytics)
### nk ref ############################
fujiadd_n100 <- readRDS(file = "refNK_n_100k_20.rds")
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
fujiadd_n100 <- readRDS(file = "refNK_n_100k_80.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k1 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p1 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k2 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p2 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k3 <- kurt
nk08 <- c(k1,k2,k3)
datakurt <- data.frame("t" = c(rep(0.2,length(nk02)),
                               rep(0.4,length(nk04)),
                               rep(0.6,length(nk06)),
                               rep(0.8,length(nk08))
),
"kurtosis" = c(nk02,nk04,nk06,nk08))
df <- data_summary(datakurt, varname="kurtosis", 
                   groupnames=c("t"))
p <- ggplot(df, aes(x = t, y = kurtosis)) +
  geom_point() +
  geom_errorbar(aes(ymin=kurtosis-sd, ymax=kurtosis+sd), color = "black", width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 3, color = "red") +
  labs(x = "phi", y = "Kurtosis")
p
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p3 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
ggarrange(p1,p2,p3, ncol = 3) 
fujiadd_n100 <- readRDS(file = "refNK_n_100k_20.rds")
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
fujiadd_n100 <- readRDS(file = "refNK_n_100k_80.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p1 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p2 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p3 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

fujiadd_n100 <- readRDS(file = "refNK_n_100k_20.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p1 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p2 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p3 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 
fujiadd_n100 <- readRDS(file = "refNK_n_100k_60.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3) 
### nk ref ############################
fujiadd_n100 <- readRDS(file = "refsmallNK_n_15k_2.rds")
kurt <- c()
phi <- c()
for (k in c(2,3,6,9,12)) {
  fujiadd_n100 <- readRDS(file = paste("refsmallNK_n_15k_", as.character(k),".rds", sep = ""))
  for (i in 1:3) {
    a <- fujiadd_n100[[i]]
    aneighbor <- a[,-1]
    aminusplus1 <- a[,-1]/a[,1]
    ku <- kurtosis(t(aminusplus1))
    kurt <- c(kurt,ku)
    phi <- c(phi,rep(k/15,length(ku)))
  }
}
datakurt <- data.frame("t" = phi,
"kurtosis" = kurt)
df <- data_summary(datakurt, varname="kurtosis", 
                   groupnames=c("t"))
p <- ggplot(df, aes(x = t, y = kurtosis)) +
  geom_point() +
  geom_errorbar(aes(ymin=kurtosis-sd, ymax=kurtosis+sd), color = "black", width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 3, color = "red") +
  labs(x = "phi", y = "Kurtosis")
p
##
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k1 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p1 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k2 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p2 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k3 <- kurt
nk04 <- c(k1,k2,k3)
datakurt <- data.frame("t" = c(rep(0.2,length(nk02)),
                               rep(0.4,length(nk04)),
                               rep(0.6,length(nk06)),
                               rep(0.8,length(nk08))
),
"kurtosis" = c(nk02,nk04,nk06,nk08))
df <- data_summary(datakurt, varname="kurtosis", 
                   groupnames=c("t"))
p <- ggplot(df, aes(x = t, y = kurtosis)) +
  geom_point() +
  geom_errorbar(aes(ymin=kurtosis-sd, ymax=kurtosis+sd), color = "black", width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 3, color = "red") +
  labs(x = "phi", y = "Kurtosis")
p
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p3 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
ggarrange(p1,p2,p3, ncol = 3) 
n <- 15
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p1 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p2 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p3 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p1 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p2 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p3 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3) 
### dim mul 0.9 adj ##################
dimmulti_n100_d0.9 <- readRDS(file = "dimin_multi_n_100dimin_0.998946949690454.rds")
a <- dimmulti_n100_d0.9[[1]]
aneighbor <- a[,2:ncol(a)]
aminusplus1 <- a[,2:ncol(a)]/a[,1] # normalize the neighbors at 1 = current #
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(a[,2:(n+1)]),
                   "fill" = as.character(rep(a[,1], n)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, alpha =0.5)) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = current_fitness), color = "grey50") +
  labs(x = "Fitness", y = "Scaled density")

data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], n)))

p2 <- ggplot(data, aes(neighbor_fitness, ..ndensity..)) +
  geom_density(aes(fill = fill, color = fill), alpha =0.07) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Fitness", y = "Scaled density") 
skew <- skewness(t(aminusplus1))
kurt <- kurtosis(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Kurtosis = kurt, Mean = mean, Skewness = skew)
p1 <- ggplot(data, aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
p2 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
p3 <- ggplot(data) +
  geom_point(aes(Step,Kurtosis)) 
### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}

hist(listamiben[[1]], breaks = 100)
plot(density(listamiben[[1]], bw = 0.0005))
hist(listamidel[[1]], breaks = 100)
hist(aminusplus1[1,], breaks = 100)

normalizedmean <- rowMeans(aminusplus1)
plot(normalizedmean)
skew <- apply(aminusplus1,1,function(x) skewness(x))
kurt <- apply(aminusplus1,1,function(x) kurtosis(x))
plot(skew)
plot(kurt)
### plot bene evo #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]
dataneut <- data[data$neighbor_fitness == 1,]
pool <- c(4,15,29,30,39,40)#sort(sample(seq(1,nrow(a),4),6))
n <- 100
plotlist <- list()
dummy <- 1
i<-1
for (i in pool) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  # Create a text
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.9,
                            y=0.9,
                            hjust=1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  # Plot
  
  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
# top = "DFE of beneficial mutation, multiplicative landscape, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.1,
                            y=0.9,
                            hjust=0,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
#top = "DFE of deleterious mutation, multiplicative landscape, n = 100")


plotlist <- list()
dummy <- 1
for (i in pool) {
  dataall <- data[data$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.9,
                            y=0.9,
                            hjust=1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
#top = "DFE of multiplicative landscape, n = 100")

### dim mul 0.5 adj
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
skew <- skewness(t(aminusplus1))
kurt <- kurtosis(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Kurtosis = kurt, Mean = mean, Skewness = skew)
p4 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
p5 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
p6 <- ggplot(data) +
  geom_point(aes(Step,Kurtosis)) 
ggarrange(p1,p4,ncol = 2,labels = c("A", "B"))

### multi, fn = 0.3 ##################
multi_n100 <- readRDS(file = "multi_n_100.rds")
a <- multi_n100[[2]]
aneighbor <- a[,2:ncol(a)]
aminusplus1 <- a[,2:ncol(a)]/a[,1] # normalize the neighbors at 1 = current #
skew <- skewness(t(aminusplus1))
kurt <- kurtosis(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Kurtosis = kurt, Mean = mean, Skewness = skew)
p1 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
p2 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
p3 <- ggplot(data) +
  geom_point(aes(Step,Kurtosis)) 
  #geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2,p3, ncol = 3)
p3
### Plot normalized data distributions #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..ndensity..)) +
  geom_density(aes(fill = fill, color = fill), alpha =0.07) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 1), color = "black") +
  labs(x = "Fitness", y = "Scaled density") 
  # #ggtitle("MFEDs during an adaptive walk, 
  #         normalized with current fitness = 1\n
  #         Multiplicative landscape, 
  #         genome size = 100, fn = 0.3, fb = 0.5")
p1

### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}

hist(listamiben[[1]], breaks = 100)
plot(density(listamiben[[1]], bw = 0.0005))
hist(listamidel[[1]], breaks = 100)
hist(aminusplus1[1,], breaks = 100)

normalizedmean <- rowMeans(aminusplus1)
plot(normalizedmean)
skew <- apply(aminusplus1,1,function(x) skewness(x))
kurt <- apply(aminusplus1,1,function(x) kurtosis(x))
plot(skew)
plot(kurt)
### plot bene evo #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]
dataneut <- data[data$neighbor_fitness == 1,]
pool <- c(4,10, 11,28,31,35)#sort(sample(seq(1,nrow(a),4),6))
n <- 100
plotlist <- list()
dummy <- 1
i<-1
for (i in pool) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  # Create a text
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.9,
                            y=0.9,
                            hjust=1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  # Plot

  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
                    # top = "DFE of beneficial mutation, multiplicative landscape, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.1,
                            y=0.9,
                            hjust=0,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
                     #top = "DFE of deleterious mutation, multiplicative landscape, n = 100")


plotlist <- list()
dummy <- 1
for (i in pool) {
  dataall <- data[data$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  grob <- grobTree(textGrob(paste("Step ",
                                  as.character(i),
                                  sep = ""), 
                            x=0.9,
                            y=0.9,
                            hjust=1,
                            gp=gpar(col="black", fontsize=11, fontface="italic")))
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    annotation_custom(grob) +
    #xlim(0.99,1.02) +
    #ylim(0,400) +
    labs(x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2)
                     #top = "DFE of multiplicative landscape, n = 100")

### fit distribution of normalized data #######
storedistpara <- matrix(nrow = nrow(aminusplus1), 
                        ncol = 6)
for (i in 1:nrow(aminusplus1)) {
  fit.gamma <- fitdist(aminusplus1[i,], distr = "gamma", method = "mle")
  storedistpara[i,1] <- i
  storedistpara[i,2] <- fit.gamma$estimate[1]
  storedistpara[i,3] <- fit.gamma$estimate[2]
  storedistpara[i,4] <- fit.gamma$estimate[1]/fit.gamma$estimate[2]
  storedistpara[i,5] <- 3 + 6/fit.gamma$estimate[1]
  storedistpara[i,6] <- 2/sqrt(fit.gamma$estimate[1])
}

storedata <- data.frame(rep(storedistpara[,1],3),
                        c(storedistpara[,4],
                          storedistpara[,5],
                          storedistpara[,6]),
                        c(rep("Mean",nrow(storedistpara)),
                          rep("Kurtosis",nrow(storedistpara)),
                          rep("Skewness",nrow(storedistpara))))
colnames(storedata) <- c("Step","Unit","Parameters")
p <- ggplot(storedata[storedata$Parameters == "Mean",]) +
  labs(x = "Step") +
  theme(legend.position = "none") +
  geom_point(aes(Step, Unit)) +
  geom_hline(aes(yintercept = 1), color = "black") +
  ggtitle("Mean")
  #ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 100, fn = 0.3, fb = 0.5")
p
p <- ggplot(storedata[storedata$Parameters == "Kurtosis",]) +
  labs(x = "Step") +
  theme(legend.position = "none") +
  geom_point(aes(Step, Unit)) +
  ggtitle("Kurtosis")
#ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 100, fn = 0.3, fb = 0.5")
p
p <- ggplot(storedata[storedata$Parameters == "Skewness",]) +
  labs(x = "Step") +
  theme(legend.position = "none") +
  geom_point(aes(Step, Unit)) +
  ggtitle("Skewness")
#ggtitle("Fitted parameters of gamma distribtion to normalized MFEDs \nMultiplicative landscape, genome size = 100, fn = 0.3, fb = 0.5")
p


#######################
### new data wo neutral in simulation #######

multi_n70 <- readRDS(file = "multi_n_70.rds")
a <- multi_n70[[9]]
aneighbor <- a[,2:ncol(a)]
aminusplus1 <- a[,2:ncol(a)]/a[,1] # normalize the neighbors at 1 = current #

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

### Plot normalized data distributions #######
data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 70)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill), alpha =0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  theme(legend.position = "none") +
  ggtitle("MFEDs during an adaptive walk, 
          normalized with current fitness = 1\n
          Multiplicative landscape, 
          genome size = 100, fn = 0.3, fb = 0.5")
p1

### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}
plot(deles)

### plot bene evo #######
data <- data.frame("current_fitness" = rep(a[,1], 70),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 70)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]

plotlist <- list()
dummy <- 1
for (i in c(1,8,16,30,36,42)) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  nfb <- benes[i]
  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    #xlim(0.99,1.035) +
    labs(title=paste("fn=0, fb= ",as.character(nfb),"/70",sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
             top = "DFE of beneficial mutation, multiplicative landscape, n = 70")

plotlist <- list()
dummy <- 1
for (i in c(1,8,16,30,36,42)) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  nfd <- deles[i]
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.97,1.002) +
    labs(title=paste("fn=0, fd= ",as.character(nfd),"/70",sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of deleterious mutation, multiplicative landscape, n = 70")

plotlist <- list()
dummy <- 1
for (i in c(1,8,16,30,36,42)) {
  dataall <- data[data$current_fitness == a[,1][i],]
  nfd <- deles[i]
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    #xlim(0.97,1.03) +
    labs(title=paste("fn=0, fd= ",as.character(nfd),"/70",sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of multiplicative landscape, n = 70")

#### draft plot ################
length(listamiben[[1]])
a1 <- hist(listamiben[[1]], 
     breaks = 100, 
     main = "DFE of beneficial mutations, step 1 
     multiplicative landscape, fn=0, fb= 47/70",
     xlab = "fitness",
     ylab = "count",
     xlim = c(1,1.032))

length(listamiben[[15]])
a2 <- hist(listamiben[[15]], 
     breaks = 100, 
     main = "DFE of beneficial mutations, step 15 
     multiplicative landscape, fn=0, fb= 33/70",
     xlab = "fitness",
     ylab = "count",
     xlim = c(1,1.032))
length(listamiben[[30]])
a3 <- hist(listamiben[[30]], 
     breaks = 100, 
     main = "DFE of beneficial mutations, step 30 
     multiplicative landscape, fn=0, fb= 18/70",
     xlab = "fitness",
     ylab = "count",
     xlim = c(1,1.032))
length(listamiben[[45]])
a4 <- hist(listamiben[[45]], 
     breaks = 100, 
     main = "DFE of beneficial mutations, step 45 
     multiplicative landscape, fn=0, fb= 3/70",
     xlab = "fitness",
     ylab = "count",
     xlim = c(1,1.032))
b1 <- recordPlot(plot(density(listamiben[[1]]),
     main = "DFE of beneficial mutations, step 1 
     multiplicative landscape",
     xlim = c(0.99,1.032)
     ))
b2 <- recordPlot(plot(density(listamiben[[15]]),
     main = "DFE of beneficial mutations, step 15 
     multiplicative landscape",
     xlim = c(0.99,1.032)
))
b3 <- recordPlot(plot(density(listamiben[[30]]),
     main = "DFE of beneficial mutations, step 30 
     multiplicative landscape",
     xlim = c(0.99,1.032)
))
b4 <- recordPlot(plot(density(listamiben[[45]]),
     main = "DFE of beneficial mutations, step 45 
     multiplicative landscape",
     xlim = c(0.99,1.032)
))

plot_grid(a1,a2,a3,a4,b1,b2,b3,b4,
             ncol =2, nrow = 4)
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

### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}

hist(listamiben[[1]], breaks = 100)
plot(density(listamiben[[1]], bw = 0.0005))
hist(listamidel[[1]], breaks = 100)
hist(aminusplus1[1,], breaks = 100)

normalizedmean <- rowMeans(aminusplus1)
plot(normalizedmean)
abline(h=1)

### plot bene evo #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]
pool <- sort(sample(seq(1,nrow(a),4),6))
n <- 100
plotlist <- list()
dummy <- 1
for (i in pool) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.998,1.014) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of beneficial mutation, diminishing-returns landscape, dimin = 0.5, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.984,1.002) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of deleterious mutation, diminishing-returns landscape, dimin = 0.5, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  dataall <- data[data$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.984,1.014) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of diminishing-returns landscape, dimin = 0.5, n = 100")

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
### fuji ref ############################
fujiadd_n100 <- readRDS(file = "fuji_add_3_n_100ratio_0.25.rds")
a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p1 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p2 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p3 <- ggplot(data,aes(Step,Mean)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 1), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 


a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k1 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p1 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k2 <- kurt
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p2 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
kurt <- kurtosis(t(aminusplus1))
k3 <- kurt
t10 <- c(k1,k2,k3)
datakurt <- data.frame("t" = c(rep(0.1,length(t01)),
                               rep(1,length(t1)),
                               rep(1.4,length(t14)),
                               rep(2.5,length(t25)),
                               rep(4,length(t4)),
                               rep(10,length(t10))
                               ),
                       "kurtosis" = c(t01,t1,t14,t25,t4,t10))
df <- data_summary(datakurt, varname="kurtosis", 
                     groupnames=c("t"))
p <- ggplot(df, aes(x = t, y = kurtosis)) +
  geom_point() +
  geom_errorbar(aes(ymin=kurtosis-sd, ymax=kurtosis+sd), color = "black", width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 3, color = "red") +
  labs(x = "theta", y = "Kurtosis")
p
step <- 1:length(kurt)
data <- data.frame(Step= step, Kurtosis = kurt)
p3 <- ggplot(data,aes(Step,Kurtosis)) +
  geom_point() 
ggarrange(p1,p2,p3, ncol = 3)

fujiadd_n100 <- readRDS(file = "fuji_add_3_n_100ratio_10.rds")
a <- fujiadd_n100[[1]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(a[,2:101]),
                   "fill" = as.character(rep(a[,1], 100)))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(color = fill, fill = fill), alpha = 0.1) +
  theme(legend.position = "none") +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = current_fitness, color = fill)) +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3                                       # Labels of the scatter plot
) 

a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p1 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p2 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p3 <- ggplot(data,aes(Step,Skewness)) +
  geom_point() +
  stat_smooth(method='lm') +
  stat_cor(label.x.npc = "left",label.y.npc = "bottom", color = "red") +
  #stat_regline_equation(label.x.npc = "left",label.y.npc = 0.02,output.type = "expression", color = "red") +
  geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

a <- fujiadd_n100[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3) 
###############################
### fuji add###############
fujiadd_n100 <- readRDS(file = "fuji_add_mf_n_100ratio_10.rds")
### normalize neighbors at 1 ########
a <- fujiadd_n100[[5]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p1 <- ggplot(data) +
  geom_point(aes(Step,Mean)) +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p2 <- ggplot(data) +
  geom_point(aes(Step,Mean)) +
  geom_hline(aes(yintercept = 1), color = "black")
a <- fujiadd_n100[[7]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p3 <- ggplot(data) +
  geom_point(aes(Step,Mean)) +
  geom_hline(aes(yintercept = 1), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

a <- fujiadd_n100[[5]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p1 <- ggplot(data) +
  geom_point(aes(Step,Skewness)) +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p2 <- ggplot(data) +
  geom_point(aes(Step,Skewness)) +
  geom_hline(aes(yintercept = 0), color = "black")
a <- fujiadd_n100[[7]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p3 <- ggplot(data) +
  geom_point(aes(Step,Skewness)) +
  geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2,p3, ncol = 3) 

data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[3]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- fujiadd_n100[[7]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p3 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2,p3, ncol = 3) 
### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}

hist(listamiben[[1]], breaks = 100)
plot(density(listamiben[[1]], bw = 0.0005))
hist(listamidel[[1]], breaks = 100)
hist(aminusplus1[1,], breaks = 100)

normalizedmean <- rowMeans(aminusplus1)
plot(normalizedmean)
abline(h=1)

### plot bene evo #######
data <- data.frame("current_fitness" = rep(a[,1], 100),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], 100)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]
pool <- sort(sample(seq(1,nrow(a)-1,1),6))
n <- 100
plotlist <- list()
dummy <- 1
for (i in pool) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.998,1.013) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of beneficial mutation, RMF landscape, phi = 0.4, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.982,1.002) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of deleterious mutation, RMF landscape, phi = 0.4, n = 100")

plotlist <- list()
dummy <- 1
for (i in pool) {
  dataall <- data[data$current_fitness == a[,1][i],]
  fd <- deles[i]/n
  fn <- neutrals[i]/n
  fb <- benes[i]/n
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.982,1.013) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of RMF landscape, phi = 0.4, n = 100")

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
NK <- readRDS(file = "NK_n_100k_80.rds")
a <- NK[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p1 <- ggplot(data) +
  geom_point(aes(Step,Mean)) +
  geom_hline(aes(yintercept = 1), color = "black")
a <- NK[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Mean = mean)
p2 <- ggplot(data) +
  geom_point(aes(Step,Mean)) +
  geom_hline(aes(yintercept = 1), color = "black")
ggarrange(p1,p2, ncol = 2) 

a <- NK[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p1 <- ggplot(data) +
  geom_point(aes(Step,Skewness)) +
  geom_hline(aes(yintercept = 0), color = "black")
a <- NK[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
skew <- skewness(t(aminusplus1))
mean <- rowMeans(aminusplus1)
step <- 1:length(mean)
data <- data.frame(Step= step, Skewness = skew)
p2 <- ggplot(data) +
  geom_point(aes(Step,Skewness)) +
  geom_hline(aes(yintercept = 0), color = "black")
ggarrange(p1,p2, ncol = 2) 

a <- NK[[1]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p1 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
a <- NK[[2]]
aneighbor <- a[,-1]
aminusplus1 <- a[,-1]/a[,1]
data <- data.frame("current_fitness" = rep(a[,1], ncol(aneighbor)),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], ncol(aneighbor))))
p2 <- ggplot(data, aes(neighbor_fitness, ..scaled..)) +
  geom_density(aes(fill = fill, color=fill), alpha = 0.1) +
  geom_rug(aes(x = neighbor_fitness, y = 0, color = fill), position = position_jitter(height = 0)) +
  geom_vline(aes(xintercept = 1), color = "black") +
  theme(legend.position = "none") +
  labs(x = "Fitness", y = "Scaled density")
ggarrange(p1,p2, ncol = 2) 

### adv del percentages ###########
binaryaminus <- aminusplus1
binaryaminus[binaryaminus < 1] <- -1
binaryaminus[binaryaminus == 1] <- 0
binaryaminus[binaryaminus > 1] <- 1
neutrals <- rowSums(binaryaminus == 0, na.rm=TRUE)
benes <- rowSums(binaryaminus == 1, na.rm=TRUE)
deles <- rowSums(binaryaminus == -1, na.rm=TRUE)
listamidel <- list()
listamiben <- list()
for (i in 1:nrow(aminusplus1)) {
  listamiben[[i]] <- aminusplus1[i,][aminusplus1[i,] > 1]
  listamidel[[i]] <- aminusplus1[i,][aminusplus1[i,] < 1]
}

hist(listamiben[[1]], breaks = 100)
plot(density(listamiben[[1]], bw = 0.0005))
hist(listamidel[[1]], breaks = 100)
hist(aminusplus1[1,], breaks = 100)

normalizedmean <- rowMeans(aminusplus1)
plot(normalizedmean)
abline(h=1)

### plot bene evo #######
n <- 100
data <- data.frame("current_fitness" = rep(a[,1], n),
                   "neighbor_fitness" = as.vector(aminusplus1),
                   "fill" = as.character(rep(a[,1], n)))
databene <- data[data$neighbor_fitness > 1,]
datadele <- data[data$neighbor_fitness < 1,]
pool <- sort(sample(seq(1,nrow(a)-1,1),6))

plotlist <- list()
dummy <- 1
for (i in pool) {
  datab <- databene[databene$current_fitness == a[,1][i],]
  fd <- round(deles[i]/n,digits = 2)
  fn <- neutrals[i]/n
  fb <- round(benes[i]/n,digits = 2)
  p <- ggplot(data=datab, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.998,1.012) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of beneficial mutation, NK landscape, n=100, k=60")

plotlist <- list()
dummy <- 1
for (i in pool) {
  datad <- datadele[datadele$current_fitness == a[,1][i],]
  fd <- round(deles[i]/n,digits = 2)
  fn <- neutrals[i]/n
  fb <- round(benes[i]/n,digits = 2)
  p <- ggplot(data=datad, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008,  
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.88,1.002) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of deleterious mutation, NK landscape, n=100, k=60")

plotlist <- list()
dummy <- 1
for (i in pool) {
  dataall <- data[data$current_fitness == a[,1][i],]
  fd <- round(deles[i]/n,digits = 2)
  fn <- neutrals[i]/n
  fb <- round(benes[i]/n,digits = 2)
  p <- ggplot(data=dataall, aes(neighbor_fitness)) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.0008, 
                   col="black", 
                   fill="green", 
                   alpha=0.2) + 
    geom_density(col=2) + 
    xlim(0.88,1.012) +
    labs(title=paste("fn = ",as.character(fn), ", fd = ",as.character(fd),", fb = ",as.character(fb),sep = ""), 
         x="fitness", 
         y="")
  plotlist[[dummy]] <- p
  dummy <- dummy +1
}

main <- grid.arrange(grobs = plotlist, nrow = 2,
                     top = "DFE of NK landscape, n=100, k=60")

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
