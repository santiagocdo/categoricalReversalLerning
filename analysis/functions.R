cleanEasy <- function () {
  # get files cotaning ".csv" from data folder
  files <- list.files("data/exp1a_easy/",pattern =  ".csv")
  
  # create vector with relevant columns to filter read csvs ("data/")
  # rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
  # create vector with relevant columns to filter read csvs ("data/pilot/")
  rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
                "response.keys","response.corr","response.rt","condition",
                "difficulty","set")
  
  # loop files, clean them and bind them into a long format dataframe
  for (i in 1:length(files)) {
    # read ith file
    temp <- read.csv(paste0("data/exp1a_easy/",files[i]))
   
    # wide format and demographics
    tempWf <- temp[1,c("participant","Age.","Gender.","Nationality.",
                       "Highest.Educational.Degree.")]
    
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
    
    # create a simpler subject ID
    temp$subjectId <- paste0("P",i)
    
    # create a simpler subject ID
    temp$goodSubject <- T
    
    # do they learn?  
    temp0 <- temp[temp$phase == 1,]
    test <- binom.test(sum(temp0$response.corr), nrow(temp0), 0.5, alternative = "greater")
    # temp0 <- temp[temp$phase == 1 & temp$task == "total",]
    # testT <- binom.test(sum(temp0$response.corr),nrow(temp0), 0.5, alternative = "greater")
    # temp0 <- temp[temp$phase == 1 & temp$task == "partial" & temp$trials.thisRepN > 4,]
    # testP <- binom.test(sum(temp0$response.corr),nrow(temp0), 0.5, alternative = "greater")
    # binom.test(212,384,0.5)
    if (test$p.value > 0.05) {
      print(paste(temp$subjectId[1],temp$participant[1]))
      temp$goodSubject <- F
    }
    
    # combine subjects
    if (i == 1) {
      lf <- temp
      wf <- tempWf
    } else {
      lf <- rbind(lf,temp)
      wf <- rbind(wf,tempWf)
    }
  }
  # add 1 trial (in python 0 is 1)
  lf$blocksPhase <- lf$trials.thisRepN + 1; lf$trials.thisRepN <- NULL
  # create sequential trials phase 2 continue trials from phase 1
  lf$blocks <- ifelse(lf$phase==1,lf$blocksPhase,lf$blocksPhase+max(lf$blocksPhase))
  # create additional features paste condition and task
  lf$condition2 <- paste0(lf$task,"-",lf$condition)
  lf$condition2 <- factor(lf$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
  # add good and bad trials
  lf$goodTrials <- ifelse(lf$response.rt > 0.2 & lf$response.rt < 7,T,F)
  
  return(list(lf=lf,wf=wf))
}

cleanHard <- function() {
  # get files cotaning ".csv" from data folder
  files <- list.files("data/exp1b_hard/",pattern =  ".csv")
  
  # create vector with relevant columns to filter read csvs ("data/")
  # rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
  # create vector with relevant columns to filter read csvs ("data/pilot/")
  rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
                "response.keys","response.corr","response.rt","condition",
                "difficulty","set")
  
  # loop files, clean them and bind them into a long format dataframe
  for (i in 1:length(files)) {
    # read ith file
    temp <- read.csv(paste0("data/exp1b_hard/",files[i]))
    
    # wide format and demographics
    tempWf <- temp[1,c("participant","Age.","Gender.","Nationality.",
                       "Highest.Educational.Degree.")]
    
    
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
    temp1 <- ifelse(temp1==0,0,1)
    temp2<-table(parPh2$Exemplars,parPh2$r_correct)
    temp2 <- ifelse(temp2==0,0,1)
    non_reversed <- rownames(temp1)[temp1[,1]==temp2[,1]]
    parPh1$condition <- ifelse(parPh1$Exemplars==non_reversed[1] |
                                 parPh1$Exemplars==non_reversed[2] |
                                 parPh1$Exemplars==non_reversed[3] |
                                 parPh1$Exemplars==non_reversed[4],
                               "nonreversed","reversed")
    parPh2$condition <- ifelse(parPh2$Exemplars==non_reversed[1] |
                                 parPh2$Exemplars==non_reversed[2] |
                                 parPh2$Exemplars==non_reversed[3] |
                                 parPh2$Exemplars==non_reversed[4],
                               "nonreversed","reversed")
    
    
    # bind the phase/tasks dataframes as a function of which task was first
    if (unique(temp$task)[1] == "partial") {
      parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "first-partial"
      temp <- rbind(parPh1,parPh2,totPh1,totPh2)
    } else {
      parPh1$first <- parPh2$first <- totPh1$first <- totPh2$first <- "first-total"
      temp <- rbind(totPh1,totPh2,parPh1,parPh2)
    }
    
    # create a simpler subject ID
    temp$subjectId <- paste0("P",i)
    
    # create a simpler subject ID
    temp$goodSubject <- T
    
    # do they learn?
    temp0 <- temp[temp$phase == 1,]
    test <- binom.test(sum(temp0$response.corr), nrow(temp0), 0.5, alternative = "greater")
    
    if (test$p.value > 0.05) {
      print(paste(temp$subjectId[1],temp$participant[1]))
      temp$goodSubject <- F
    }
    
    # combine subjects
    if (i == 1) {
      lf <- temp
      wf <- tempWf
    } else {
      lf <- rbind(lf,temp)
      wf <- rbind(wf,tempWf)
    }
  }
  # add 1 trial (in python 0 is 1)
  lf$blocksPhase <- lf$trials.thisRepN + 1; lf$trials.thisRepN <- NULL
  # create sequential trials phase 2 continue trials from phase 1
  lf$blocks <- ifelse(lf$phase==1,lf$blocksPhase,lf$blocksPhase+max(lf$blocksPhase))
  # create additional features paste condition and task
  lf$condition2 <- paste0(lf$task,"-",lf$condition)
  lf$condition2 <- factor(lf$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
  # add good and bad trials
  lf$goodTrials <- ifelse(lf$response.rt > 0.2 & lf$response.rt < 7,T,F)
  
  return(list(lf=lf,wf=wf))
}


# # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
combineSimConditions <- function(par,tot) {
  # ggplot(tot, aes(x=nBlock,y=actO,col=trialType)) +
  #   stat_summary(geom = "line") +
  #   facet_grid(.~out) +
  #   theme_classic()
  # ggplot(par, aes(x=nBlock,y=actO,col=trialType)) +
  #   stat_summary(geom = "line") +
  #   facet_grid(.~out) +
  #   theme_classic()
  
  # prepare phase 2 partial reversal only reversed stimuli
  # par <- par[par$ph == 2,]
  actO.1 <- par[par$out == "actO.1",]
  actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
  actO.2 <- par[par$out == "actO.2",]
  par <- data.frame(actO.1,actO.2=actO.2$actO)
  
  par$discScore[par$ph==1] <- ifelse(par$trialType[par$ph==1] == "A" | par$trialType[par$ph==1] == "B" |
                                       par$trialType[par$ph==1] == "C" | par$trialType[par$ph==1] == "D",
                                     par$actO.1[par$ph==1]/(par$actO.1[par$ph==1]+par$actO.2[par$ph==1]),
                                     par$actO.2[par$ph==1]/(par$actO.1[par$ph==1]+par$actO.2[par$ph==1]))
  par$discScore[par$ph==2] <- ifelse(par$trialType[par$ph==2] == "A" | par$trialType[par$ph==2] == "B" |
                                       par$trialType[par$ph==2] == "G" | par$trialType[par$ph==2] == "H",
                                     par$actO.1[par$ph==2]/(par$actO.1[par$ph==2]+par$actO.2[par$ph==2]),
                                     par$actO.2[par$ph==2]/(par$actO.1[par$ph==2]+par$actO.2[par$ph==2]))
  par$condition <- ifelse(par$trialType == "C" | par$trialType == "D" |
                            par$trialType == "G" | par$trialType == "H",
                          "reversed","nonreversed")
  # ggplot(par, aes(x=nBlock,y=actO.1/(actO.2+actO.1),col=trialType)) +
  #   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, alpha = 0.1) +
  #   stat_summary(fun = "mean", geom="line")
  # par$condition[par$ph==1] <- ifelse(par$trialType[par$ph==1] == "C" | par$trialType[par$ph==1] == "D" |
  #                                      par$trialType[par$ph==1] == "G" | par$trialType[par$ph==1] == "H",
  #                                    "reversal","non-reversal")
  # par$condition[par$ph==2] <- ifelse(par$trialType[par$ph==2] == "C" | par$trialType[par$ph==2] == "D" |
  #                                      par$trialType[par$ph==2] == "G" | par$trialType[par$ph==2] == "H",
  #                                    "reversal","non-reversal")

  # relevant <- t(matrix(c("C","actO.2",
  #                        "D","actO.2",
  #                        "G","actO.1",
  #                        "H","actO.1"),ncol=4)); par$relevant <- F
  # for (i in 1:nrow(relevant)) {
  #   par$relevant[par$trialType == relevant[i,1] & par$out == relevant[i,2]] <- T
  # }
  # par <- par[par$relevant == T,]; par$relevant <- NULL
  
  
  
  # prepare phase 2 total reversal
  # tot <- tot[tot$ph == 2,]
  actO.1 <- tot[tot$out == "actO.1",]
  actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
  actO.2 <- tot[tot$out == "actO.2",]
  tot <- data.frame(actO.1,actO.2=actO.2$actO)
  
  tot$discScore[tot$ph==1] <- ifelse(tot$trialType[tot$ph==1] == "E" | tot$trialType[tot$ph==1] == "F" |
                                       tot$trialType[tot$ph==1] == "G" | tot$trialType[tot$ph==1] == "H",
                                     tot$actO.2[tot$ph==1]/(tot$actO.1[tot$ph==1]+tot$actO.2[tot$ph==1]),
                                     tot$actO.1[tot$ph==1]/(tot$actO.1[tot$ph==1]+tot$actO.2[tot$ph==1]))
  tot$discScore[tot$ph==2] <- ifelse(tot$trialType[tot$ph==2] == "E" | tot$trialType[tot$ph==2] == "F" |
                                       tot$trialType[tot$ph==2] == "G" | tot$trialType[tot$ph==2] == "H",
                                     tot$actO.1[tot$ph==2]/(tot$actO.1[tot$ph==2]+tot$actO.2[tot$ph==2]),
                                     tot$actO.2[tot$ph==2]/(tot$actO.1[tot$ph==2]+tot$actO.2[tot$ph==2]))
  tot$condition <- "reversed"
  
  # relevant <- t(matrix(c("A","actO.2",
  #                        "B","actO.2",
  #                        "C","actO.2",
  #                        "D","actO.2",
  #                        "E","actO.1",
  #                        "F","actO.1",
  #                        "G","actO.1",
  #                        "H","actO.1"),ncol=8)); tot$relevant <- F
  # for (i in 1:nrow(relevant)) {
  #   tot$relevant[tot$trialType == relevant[i,1] & tot$out == relevant[i,2]] <- T
  # }
  # tot <- tot[tot$relevant == T,]; tot$relevant <- NULL
  
  
  
  # combine both databases
  db <- rbind(par,tot)
  return(db)
}
