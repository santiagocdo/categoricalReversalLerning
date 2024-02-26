# get files cotaning ".csv" from data folder
files <- list.files("data/pilot/",pattern =  ".csv")

# create vector with relevant columns to filter read csvs ("data/")
# rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
# create vector with relevant columns to filter read csvs ("data/pilot/")
rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
              "response.keys","response.corr","condition","difficulty","set")

# loop files, clean them and bind them into a long format dataframe
for (i in 1:length(files)) {
  # read ith file
  temp <- read.csv(paste0("data/pilot/",files[i]))

  # clean rows and get only relevant columns
  temp <- temp[!is.na(temp$phase),rel_cols]
  
  # exemplars to only numbers
  temp$Exemplars <- as.numeric(gsub("\\D", "", temp$Exemplars))
  
  # # split by phases and task so we add the corresponding conditions
  # # total task
  # totPh1 <- temp[temp$phase==1 & temp$task=="total",]
  # totPh2 <- temp[temp$phase==2 & temp$task=="total",]
  # # add conditions
  # totPh1$condition <- "training"
  # totPh2$condition <- "reversed"
  # 
  # # table(totPh1$Exemplars,totPh1$r_correct)
  # # table(totPh2$Exemplars,totPh2$r_correct)
  # 
  # # partial task
  # parPh1 <- temp[temp$phase==1 & temp$task=="partial",]
  # parPh2 <- temp[temp$phase==2 & temp$task=="partial",]
  # # add conditions
  # parPh1$condition <- "training"
  # # for the non reversed and reversed in the partial we need tow work a bit
  # temp1<-table(parPh1$Exemplars,parPh1$r_correct)
  # temp2<-table(parPh2$Exemplars,parPh2$r_correct)
  # non_reversed <- rownames(temp1)[temp1[,1]==temp2[,1]]
  # parPh2$condition <- ifelse(parPh2$Exemplars==non_reversed[1] | 
  #                              parPh2$Exemplars==non_reversed[2] |
  #                              parPh2$Exemplars==non_reversed[3] |
  #                              parPh2$Exemplars==non_reversed[4],
  #                            "nonreversed","reversed")
  # 
  # # bind the phase/tasks dataframes as a function of which task was first
  # if (unique(temp$task)[1] == "partial") {
  #   parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "partial"
  #   temp <- rbind(parPh1,parPh2,totPh1,totPh2)
  # } else {
  #   parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "total"
  #   temp <- rbind(totPh1,totPh2,parPh1,parPh2)
  # }
    
  if (i == 1) {
    lf <- temp
  } else {
    lf <- rbind(lf,temp)
  }
}
# add 1 trial (in python 0 is 1)
lf$trials.thisRepN <- lf$trials.thisRepN + 1
# create sequential trials phase 2 continue trials from phase 1
lf$trials <- ifelse(lf$phase==1,lf$trials.thisRepN,lf$trials.thisRepN+max(lf$trials.thisRepN))


# visualize using ggplot package
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
ggplot(lf, aes(x=trials.thisRepN,y=response.corr,col=task)) +
  labs(x="blocks", y="p(correct)") +
  geom_hline(yintercept = 0.5) +
  stat_summary(geom="line",position = position_dodge(0.1)) +
  stat_summary(geom="errorbar",position = position_dodge(0.1)) +
  facet_grid(participant ~ phase) +
  theme_bw()

lf$condition <- factor(lf$condition, levels = c("training","reversed","nonreversed"))
ggplot(lf, aes(x=trials.thisRepN,y=response.corr,col=task)) +
  labs(title = "N=5",x="blocks", y="p(correct)") +
  geom_hline(yintercept = 0.5) +
  stat_summary(geom="line",position = position_dodge(0.1)) +
  stat_summary(geom="errorbar",position = position_dodge(0.1)) +
  facet_grid(. ~ condition) +
  theme_bw()
