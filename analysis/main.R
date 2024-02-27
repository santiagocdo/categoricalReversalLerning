# get files cotaning ".csv" from data folder
files <- list.files("data/pilot/",pattern =  ".csv")

# create vector with relevant columns to filter read csvs ("data/")
# rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
# create vector with relevant columns to filter read csvs ("data/pilot/")
rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
              "response.keys","response.corr","response.rt","condition",
              "difficulty","set")

# loop files, clean them and bind them into a long format dataframe
for (i in 1:length(files)) {
  # read ith file
  temp <- read.csv(paste0("data/pilot/",files[i]))

  # clean rows and get only relevant columns
  temp <- temp[!is.na(temp$phase),rel_cols]
  
  # exemplars to only numbers
  temp$Exemplars <- as.numeric(gsub("\\D", "", temp$Exemplars))
  
  # split by phases and task so we add the corresponding conditions
  # total task
  totPh1 <- temp[temp$phase==1 & temp$task=="total",]
  totPh2 <- temp[temp$phase==2 & temp$task=="total",]
  # add conditions
  totPh1$condition <- "reversed"# "training"
  totPh2$condition <- "reversed"

  # table(totPh1$Exemplars,totPh1$r_correct)
  # table(totPh2$Exemplars,totPh2$r_correct)

  # partial task
  parPh1 <- temp[temp$phase==1 & temp$task=="partial",]
  parPh2 <- temp[temp$phase==2 & temp$task=="partial",]
  # add conditions
  # parPh1$condition <- "training"
  # for the non reversed and reversed in the partial we need tow work a bit
  temp1<-table(parPh1$Exemplars,parPh1$r_correct)
  temp2<-table(parPh2$Exemplars,parPh2$r_correct)
  non_reversed <- rownames(temp1)[temp1[,1]==temp2[,1]]
  # parPh2$condition <- ifelse(parPh2$Exemplars==non_reversed[1] |
  #                              parPh2$Exemplars==non_reversed[2] |
  #                              parPh2$Exemplars==non_reversed[3] |
  #                              parPh2$Exemplars==non_reversed[4],
  #                            "nonreversed","reversed")
  parPh1$condition <- ifelse(parPh1$Exemplars==non_reversed[1] |
                               parPh1$Exemplars==non_reversed[2],
                             "nonreversed","reversed")
  parPh2$condition <- ifelse(parPh2$Exemplars==non_reversed[1] |
                               parPh2$Exemplars==non_reversed[2],
                             "nonreversed","reversed")
 

  # bind the phase/tasks dataframes as a function of which task was first
  if (unique(temp$task)[1] == "partial") {
    parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "first-partial"
    temp <- rbind(parPh1,parPh2,totPh1,totPh2)
  } else {
    parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "first-total"
    temp <- rbind(totPh1,totPh2,parPh1,parPh2)
  }
    
  if (i == 1) {
    lf <- temp
  } else {
    lf <- rbind(lf,temp)
  }
}
# add 1 trial (in python 0 is 1)
lf$blocksPhase <- lf$trials.thisRepN + 1; lf$trials.thisRepN <- NULL
# create sequential trials phase 2 continue trials from phase 1
lf$blocks <- ifelse(lf$phase==1,lf$blocksPhase,lf$blocksPhase+max(lf$blocksPhase))
# create additional features paste condition and task
lf$condition2 <- paste0(lf$task,"-",lf$condition)
lf$condition2 <- factor(lf$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))

# visualize using ggplot package
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)


# visualize all subjects

# correctness
p1 <- ggplot(lf, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and\n Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(participant ~ .) +
  theme_bw()
# reaction time
p2 <- ggplot(lf[lf$response.rt < 7,], aes(x=blocks,y=response.rt,col=condition2,shape=task)) +
  labs(x="blocks", y="RT (sec.)",col="Task and\n Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_wrap(participant ~ .) +
  theme_bw()


# visualize average

# correctness
p3 <- ggplot(lf, aes(x=blocks,y=response.corr,col=condition2)) +
  labs(title = "N=32",x="blocks", y="p(correct)",col="Task and\n Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_grid(. ~ first) +
  theme_bw()

# reaction time
p4 <- ggplot(lf[lf$response.rt < 7,], aes(x=blocks,y=response.rt,col=condition2)) +
  labs(title = "N=32",x="blocks", y="RT (sec.)",col="Task and\n Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=2)) +
  stat_summary(geom="line",position = position_dodge(0.2)) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_grid(. ~ first) +
  theme_bw()


# get only relevant data to test hypothesis
temp <- lf[lf$phase==2 & lf$condition!="nonreversed",]
library(lmerTest)
# all blocks
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m)
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m)


# participant vector
participants <- unique(temp$participant)


# collapse by blocks
library(dplyr)
collapsed <- as.data.frame(lf %>% group_by(participant,first,task,difficulty,set,phase,
                                      condition2,blocks,blocksPhase) %>% 
  summarize(pCorrect=mean(response.corr,na.rm=T),mRt=mean(response.rt,na.rm=T)))

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
for (i in 1:length(participants)) {
  temp <- collapsed[collapsed$participant==participants[i],]
  temp <- temp[order(temp$condition2,temp$blocks),]
  

  corr <- data.frame(t(temp$pCorrect))
  colnames(corr) <- paste0(temp$blocks,"-",temp$condition2)
  rt <- data.frame(t(temp$mRt))
  colnames(rt) <- paste0(temp$blocks,"-",temp$condition2)
  if (i == 1) {
    wf_corr <- cbind(temp[1,relCols],corr)
    wf_rt <- cbind(temp[1,relCols],rt)
  } else {
    wf_corr <- rbind(wf_corr,cbind(temp[1,relCols],corr))
    wf_rt <- rbind(wf_rt,cbind(temp[1,relCols],rt))
  }
}
write.csv(wf_corr,"data/corr_average_blocks.csv",row.names = F)
write.csv(wf_rt,"data/rt_average_blocks.csv",row.names = F)
# write.csv(lf,"data/raw_all_participants.csv",row.names = F)



