# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# call my functions
source("analysis/functions.R")

# pool and clean easy experiment
lfe <- cleanEasy()
wfe <- lfe$wf
lfe <- lfe$lf

# pool and clean hard experiment
lfh <- cleanHard()
wfh <- lfh$wf
lfh <- lfh$lf

# print long wide format
# write.csv(lfe,"data/lf_easy.csv",row.names = F)
# write.csv(lfh,"data/lf_hard.csv",row.names = F)

# general characteristics
mean(wfe$Age.,na.rm=T); sd(wfe$Age.,na.rm=T); range(wfe$Age.,na.rm=T)
table(tolower(wfe$Gender.))
mean(wfh$Age.,na.rm=T); sd(wfh$Age.,na.rm=T); range(wfh$Age.,na.rm=T)
table(tolower(wfh$Gender.))

# visualize using ggplot package
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(blme)) {install.packages('blme')}; library(blme)
if (!require(emmeans)) {install.packages("emmeans")}; library(emmeans)
if (!require(cowplot)) {install.packages("cowplot")}; library(cowplot)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)

# remove bad trials 
lfe <- lfe[lfe$goodTrials==T,]
lfh <- lfh[lfh$goodTrials==T,]

# remove bad participants 
lfe <- lfe[lfe$goodSubject==T,]
lfh <- lfh[lfh$goodSubject==T,]

# transform blocks for stats (so intercept make sense (block 1 = 0)
lfe$blocksStats <- lfe$blocksPhase - 1
lfh$blocksStats <- lfh$blocksPhase - 1



# Gregynog 2024 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

figA <- ggplot(lfe[lfe$goodTrials == T,], 
               aes(x=blocks,y=response.corr,
                   col=condition2,shape=condition2,linetype=condition2)) +
  labs(title="Easy version", x="blocks", y="p(correct)",
       col="Task and Condition", shape="Task and Condition",
       linetype="Task and Condition") +
  coord_cartesian(ylim = c(0,1)) +
  geom_vline(xintercept = 6.5, col="black", alpha=0.5) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_shape_manual(values = c(15,19,21)) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  stat_summary(geom="point",position = position_dodge(0.2),size=3,fill="white") +
  # facet_grid(. ~ first) +
  theme_bw()
# figA

figB <- ggplot(lfh[lfh$goodTrials == T,], 
               aes(x=blocks,y=response.corr,
                   col=condition2,shape=condition2,linetype=condition2)) +
  labs(title="Hard version", x="blocks", y="p(correct)",
       col="Task and Condition", shape="Task and Condition",
       linetype="Task and Condition") +
  coord_cartesian(ylim = c(0,1)) +
  geom_vline(xintercept = 8.5, col="black", alpha=0.5) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_shape_manual(values = c(15,19,21)) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  stat_summary(geom="point",position = position_dodge(0.2),size=3,fill="white") +
  # facet_grid(. ~ first) +
  theme_bw()
# figB



# read output data
# par <- read.csv("../ALANN/figures/mod2_50t_n16_rho0.05_mu0.05_partial/exp_partial_mod2_n16.csv")
par <- read.csv("../ALANN/output/exp_partial_mod2_n32_beta0.9_rho0.01_mu0.01.csv")
par$task <- "partial"
# tot <- read.csv("../ALANN/figures/mod2_50t_n16_rho0.05_mu0.05_total/exp_total_mod2_n16.csv")
tot <- read.csv("../ALANN/output/exp_total_mod2_n32_beta0.9_rho0.01_mu0.01.csv")
tot$task <- "total"

source("../ALANN/functions.R")
db <- f_combine_sims(par,tot) 



# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
figC <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(title = expression(rho==0.01*`;`~mu==0.01),
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 200.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,400,by=100)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  theme_bw() 
# figC

# read output data
# par <- read.csv("../ALANN/figures/mod2_50t_n16_rho0.05_mu0.01_partial/exp_partial_mod2_n16.csv")
par <- read.csv("../ALANN/output/exp_partial_mod2_n32_beta0.9_rho0.01_mu1e-04.csv")
par$task <- "partial"
# tot <- read.csv("../ALANN/figures/mod2_50t_n16_rho0.05_mu0.01_total/exp_total_mod2_n16.csv")
tot <- read.csv("../ALANN/output/exp_total_mod2_n32_beta0.9_rho0.01_mu1e-04.csv")
tot$task <- "total"

db <- f_combine_sims(par,tot) 



# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
figD <- ggplot(db[,], aes(x=nBlock,y=discScore,
                          col=condition2,linetype=condition2)) +
  labs(title = expression(rho==0.01*`;`~mu==0.0001),
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 200.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,400,by=100)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  theme_bw() 
# figD




plot <- ggarrange(figA,figC,figB,figD,nrow=2,ncol=2,common.legend = T,
                  labels = c("A","C","B","D"))
# plot

ggsave(paste0("../ALANN/figures/fig2.png"), dpi = 2400, limitsize = TRUE,
       plot = plot,
       units = "in",
       width = 6, #6000
       height = 4.5) #4800






# # split by conditions
# total_reversed <- temp[temp$condition2 == "total-reversed",]
# partial_reversed <- temp[temp$condition2 == "partial-reversed",]
# partial_unreversed <- temp[temp$condition2 == "partial-nonreversed",]
# # do they have the same order? 
# sum(total_reversed$participant == partial_reversed$participant)
# sum(partial_reversed$participant == partial_unreversed$participant)
# # combine
# collapsed <- cbind(total_reversed[,],partial_reversed[,c("pCorrect","mRt")],partial_unreversed[,c("pCorrect","mRt")])
# collapsed$condition2 <- NULL


relCols <- c("participant","first","difficulty","set")
# reorder in wide format data base
for (i in 1:length(participantsE)) {
  # one participant
  temp <- collapsedE[collapsedE$participant==participantsE[i],]
  
  if (nrow(temp) > 0) {
    # in order
    temp <- temp[order(temp$condition2,temp$blocks),]
    
    corr <- data.frame(t(temp$pCorrect))
    colnames(corr) <- paste0(temp$blocks,"-",temp$condition2)
    rt <- data.frame(t(temp$medRt))
    colnames(rt) <- paste0(temp$blocks,"-",temp$condition2)
    if (i == 1) {
      wf_corr <- cbind(temp[1,relCols],corr)
      wf_rt <- cbind(temp[1,relCols],rt)
    } else {
      wf_corr <- rbind(wf_corr,cbind(temp[1,relCols],corr))
      wf_rt <- rbind(wf_rt,cbind(temp[1,relCols],rt))
    }
  }
}
write.csv(wf_corr,"data/corr_average_blocks_easy.csv",row.names = F)
write.csv(wf_rt,"data/rt_median_blocks_easy.csv",row.names = F)


relCols <- c("participant","first","difficulty","set")
# reorder in wide format data base
for (i in 1:length(participantsH)) {
  # one participant
  temp <- collapsedH[collapsedH$participant==participantsH[i],]
  
  if (nrow(temp) > 0) {
    # in order
    temp <- temp[order(temp$condition2,temp$blocks),]
    
    corr <- data.frame(t(temp$pCorrect))
    colnames(corr) <- paste0(temp$blocks,"-",temp$condition2)
    rt <- data.frame(t(temp$medRt))
    colnames(rt) <- paste0(temp$blocks,"-",temp$condition2)
    if (i == 1) {
      wf_corr <- cbind(temp[1,relCols],corr)
      wf_rt <- cbind(temp[1,relCols],rt)
    } else {
      wf_corr <- rbind(wf_corr,cbind(temp[1,relCols],corr))
      wf_rt <- rbind(wf_rt,cbind(temp[1,relCols],rt))
    }
  }
}
write.csv(wf_corr,"data/corr_average_blocks_hard.csv",row.names = F)
write.csv(wf_rt,"data/rt_median_blocks_hard.csv",row.names = F)
