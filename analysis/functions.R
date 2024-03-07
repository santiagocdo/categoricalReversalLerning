clean_easy <- function () {
  # get files cotaning ".csv" from data folder
  files <- list.files("data/pilot_easy/",pattern =  ".csv")
  
  # create vector with relevant columns to filter read csvs ("data/")
  # rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
  # create vector with relevant columns to filter read csvs ("data/pilot/")
  rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
                "response.keys","response.corr","response.rt","condition",
                "difficulty","set")
  
  # loop files, clean them and bind them into a long format dataframe
  for (i in 1:length(files)) {
    # read ith file
    temp <- read.csv(paste0("data/pilot_easy/",files[i]))
    
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
    test <- binom.test(sum(temp$response.corr),nrow(temp),0.5,alternative = "greater")
    
    if (test$p.value > 0.05) {
      print(paste(temp$subjectId[1],temp$participant[1]))
      temp$goodSubject <- F
    }
    
    # combine subjects
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
  # add good and bad trials
  lf$goodTrials <- ifelse(lf$response.rt > 0.2 & lf$response.rt < 7,T,F)
  
  return(lf)
}

clean_hard <- function() {
  # get files cotaning ".csv" from data folder
  files <- list.files("data/pilot_hard/",pattern =  ".csv")
  
  # create vector with relevant columns to filter read csvs ("data/")
  # rel_cols <- c("participant","task","phase","Exemplars","r_correct","response.keys","trials.thisRepN","response.corr")
  # create vector with relevant columns to filter read csvs ("data/pilot/")
  rel_cols <- c("participant","task","phase","Exemplars","r_correct","trials.thisRepN",
                "response.keys","response.corr","response.rt","condition",
                "difficulty","set")
  
  # loop files, clean them and bind them into a long format dataframe
  for (i in 1:length(files)) {
    # read ith file
    temp <- read.csv(paste0("data/pilot_hard/",files[i]))
    
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
    temp0 <- temp[temp$trials.thisRepN != 0,]
    test <- binom.test(sum(temp0$response.corr),nrow(temp0),0.5,alternative = "greater")
    
    if (test$p.value > 0.05) {
      print(paste(temp$subjectId[1],temp$participant[1]))
      temp$goodSubject <- F
    }
    
    # combine subjects
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
  # add good and bad trials
  lf$goodTrials <- ifelse(lf$response.rt > 0.2 & lf$response.rt < 7,T,F)
  
  return(lf)
}

