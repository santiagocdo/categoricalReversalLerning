# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# call my functions
source("analysis/functions.R")

lfe <- clean_easy()
lfh <- clean_hard()

# visualize using ggplot package
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)



# visualize all subjects

# correctness
pEasyCorr <- ggplot(lfe, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(subjectId ~ .) +
  theme_bw()
pHardCorr <- ggplot(lfh, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  geom_hline(yintercept = 0.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(subjectId ~ .) +
  theme_bw()

# reaction time
pEasyRT <- ggplot(lfe[lfe$goodTrials==T,], aes(x=blocks,y=response.rt,col=condition2,shape=task)) +
  labs(x="blocks", y="RT (sec.)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",fun = median,position = position_dodge(0.2)) +
  # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(subjectId ~ .) +
  theme_bw()
pHardRT <- ggplot(lfh[lfh$goodTrials==T,], aes(x=blocks,y=response.rt,col=condition2,shape=task)) +
  labs(x="blocks", y="RT (sec.)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",fun = median,position = position_dodge(0.2)) +
  # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(subjectId ~ .) +
  theme_bw()



# visualize average

# correctness
figA <- ggplot(lfe[lfe$goodTrials == T,], aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.25)) +
  coord_cartesian(ylim = c(0.25,0.9)) +
  scale_color_manual(values = c("red","blue","green")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = c(0.2,0.2),
                     legend.background = element_blank())
figD <- ggplot(lfh[lfh$goodTrials == T,], aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.25)) +
  coord_cartesian(ylim = c(0.25,0.9)) +
  scale_color_manual(values = c("red","blue","green")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = "none")



# reaction time
# p4e <- ggplot(lfe[lfe$goodTrials == T,], aes(x=blocks,y=response.rt,col=condition2)) +
#   labs(x="blocks", y="Median RT (sec.)",col="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = seq(0,2,by=0.25)) +
#   coord_cartesian(ylim = c(0.45,1.3)) +
#   scale_color_manual(values = c("red","blue","green")) +
#   stat_summary(geom="line",fun=median,position = position_dodge(0.2),
#                size=1.8) +
#   stat_summary(geom="errorbar",fun.data = iqr,position = position_dodge(0.2)) +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
# p4h <- ggplot(lfh[lfh$goodTrials == T,], aes(x=blocks,y=response.rt,col=condition2)) +
#   labs(x="blocks", y="Median RT (sec.)",col="Task and Condition") +
#   geom_vline(xintercept = 8.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = seq(0,2,by=0.25)) +
#   coord_cartesian(ylim = c(0.45,1.3)) +
#   scale_color_manual(values = c("red","blue","green")) +
#   stat_summary(geom="line",fun=median,position = position_dodge(0.2),
#                size=1.8) +
#   stat_summary(geom="errorbar",fun.data = iqr,position = position_dodge(0.2)) +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")



# get only relevant data to test hypothesis
temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]
figC <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.25)) +
  coord_cartesian(ylim = c(0.25,0.85)) +
  scale_color_manual(values = c("red","blue")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = "none")
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
# all blocks
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m)
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m)



# get only relevant data to test hypothesis
temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]
figF <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.25)) +
  coord_cartesian(ylim = c(0.25,0.85)) +
  scale_color_manual(values = c("red","blue")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = "none")
# all blocks
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m)
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m)



# participant vector for easy experiment
participantsE <- unique(lfe$participant)

# collapse by blocks
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
# collapsed <- as.data.frame(lf %>% group_by(participant,subjectId,first,task,difficulty,
#                                            set,phase,goodTrials) %>% 
#                              summarize(N=n()))
# temp <- collapsed[collapsed$goodTrials==F,]

# collapsed trials to get median RT for the easy experiment
collapsedE <- as.data.frame(lfe[lfe$goodSubject==T & lfe$goodTrials==T,] %>% 
                             group_by(participant,subjectId,first,task,difficulty,
                                      set,phase,condition2,blocks,blocksPhase) %>%
                             summarize(nTrials=n(),
                                       pCorrect=mean(response.corr,na.rm=T),
                                       avgRt=mean(response.rt,na.rm=T),
                                       medRt=median(response.rt,na.rm=T)))
# figure B
figB <- ggplot(collapsedE, aes(x=blocks,y=medRt,col=condition2)) +
  labs(x="blocks", y="Mean of Medians RT (sec.)",col="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,2,by=0.25)) +
  coord_cartesian(ylim = c(0.6,1.25)) +
  scale_color_manual(values = c("red","blue","green")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = "none")


# participant vector for hard experiment 
participantsH <- unique(lfh$participant)

# collapsed trials to get median RT for the hard experiment
collapsedH <- as.data.frame(lfh[lfh$goodSubject==T & lfh$goodTrials==T,] %>% 
                              group_by(participant,subjectId,first,task,difficulty,
                                       set,phase,condition2,blocks,blocksPhase) %>%
                              summarize(nTrials=n(),
                                        pCorrect=mean(response.corr,na.rm=T),
                                        avgRt=mean(response.rt,na.rm=T),
                                        medRt=median(response.rt,na.rm=T)))
# figure E
figE <- ggplot(collapsedH, aes(x=blocks,y=medRt,col=condition2)) +
  labs(x="blocks", y="Mean of Medians RT (sec.)",col="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = seq(0,2,by=0.25)) +
  coord_cartesian(ylim = c(0.6,1.25)) +
  scale_color_manual(values = c("red","blue","green")) +
  stat_summary(geom="line",position = position_dodge(0.2),
               size=1.8) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  # facet_grid(. ~ first) +
  theme_bw() + theme(legend.position = "none")





left <- annotate_figure(ggarrange(figA,figB,figC,nrow=3,
                                  labels = c("A","B","C")),
                        top = text_grob("Easy", color = "black", face = "bold", size = 14))
right <- annotate_figure(ggarrange(figD,figE,figF,nrow=3,
                                   labels = c("D","E","F")),
                         top = text_grob("Hard", color = "black", face = "bold", size = 14))

figure2 <- ggarrange(left,right,ncol=2,common.legend = T)
figure2


# combine
print_fig <- 1
if (print_fig == 1) {
  ggsave("analysis/figure2.pdf",
         plot = figure2, width = 24, height = 24, units = "cm", dpi = 900, 
         limitsize = T)
}





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
