# Remove all of the elements currently loaded in R
rm(list = ls(all = TRUE))


library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)



# list of files
files <- list.files("exemplars_similarity/data/")
# keep only the ones with "CatSimMDS" and ".csv"
files <- files[grepl("CatSimMDS",files) & grepl(".csv",files)]

# exemplars category
exs <- paste0("Ex",1:16)

# for loop over the length of files so we combine the participants data
for (i in 1:length(files)) {
  # read one participant
  oneSubj <- read.csv(paste0("exemplars_similarity/data/",files[i]))
  
  # sum of seconds in the whole task
  task_duration_sec <- sum(oneSubj[,c("resp_consent.rt","resp_instructions.rt","ready_task.rt","slider.rt","resp_end.rt")],na.rm = T)
  
  tempWf <- oneSubj[1,c("PROLIFIC_PID","Age.","Gender.","Nationality.",
                       "Highest.Educational.Degree.")]
  # remove rows with no response
  oneSubj <- oneSubj[!is.na(oneSubj$slider.response),]
  # combine
  if (i == 1) {
    allSubj <- data.frame(oneSubj,partId=paste0("parID_",i)) 
    wf <- data.frame(tempWf,task_duration_sec)
  } else {
    allSubj <- rbind(allSubj,data.frame(oneSubj,partId=paste0("parID_",i)) )
    wf <- rbind(wf,data.frame(tempWf,task_duration_sec))
  }
}

# general characteristics
mean(wf$Age.,na.rm=T); sd(wf$Age.,na.rm=T); range(wf$Age.,na.rm=T)
table(tolower(wf$Gender.))

mean(wf$task_duration_sec/60); sd(wf$task_duration_sec/60)

# left and right as ordered factors
allSubj$left <- factor(allSubj$left, levels = exs) 
allSubj$right <- factor(allSubj$right, levels = exs) 
table(allSubj$left,allSubj$right)



ggplot(allSubj, aes(x=slider.response)) + 
  geom_histogram(bins = 20, color="black", fill="white") +
  facet_wrap(partId ~ .)

# relevant columns
relCols <- c("partId","PROLIFIC_PID","Gender.","Nationality.","Highest.Educational.Degree.",
             "date","task","trials.thisTrialN","left","right","slider.response","slider.rt")
# filter data.frame
allSubj <- allSubj[,relCols]
# change column names
colnames(allSubj) <- c("partId","PROLIFIC_PID","gender","nationality","education","date",
                       "task","trial","left","right","rating","rt")
# add +1 to trials
allSubj$trial <- allSubj$trial + 1

# calculate the highest comparison to use as supplementary materials in paper
allSubj$comparison <- paste0(allSubj$left,"_vs_", allSubj$right)
sumAllSubj <- allSubj %>% group_by(partId, comparison) %>% 
  summarise(mean=mean(comparison))




# write csv in long format
# write.csv(allSubj, "exemplars_similarity/longFormat.csv",row.names = F)

# create empty matrix for output
outputMatrix <- matrix(NA, nrow = length(exs), ncol = length(exs))
rownames(outputMatrix) <- colnames(outputMatrix) <- exs
for (i in 1:length(exs)) {
  for (j in 1:length(exs)) {
    filter <- allSubj$left == exs[i] & allSubj$right == exs[j]
    if (any(filter)) {
      outputMatrix[i,j] <- mean(allSubj$rating[filter])
    }
  }
}

# write csv in long format
# write.csv(outputMatrix, "exemplars_similarity/similarityMatrixAverage.csv",row.names = F)



# create empty matrix for output
outputMatrix <- matrix(NA, nrow = length(exs), ncol = length(exs))
rownames(outputMatrix) <- colnames(outputMatrix) <- exs

# participant Ids
partId <- unique(allSubj$partId)

for (k in 1:length(partId)) {
  for (i in 1:length(exs)) {
    for (j in 1:length(exs)) {
      filter <- allSubj$left == exs[i] & allSubj$right == exs[j] & allSubj$partId == partId[k]
      if (any(filter)) {
        outputMatrix[i,j] <- mean(allSubj$rating[filter])
      }
    }
  }
  if (k == 1) {
    outputMatrixAllSubjs <- data.frame(partId=partId[k],outputMatrix)
  } else {
    outputMatrixAllSubjs <- rbind(outputMatrixAllSubjs, data.frame(partId=partId[k],outputMatrix))
  }
}
outputMatrixAllSubjs <- data.frame(partId=outputMatrixAllSubjs$partId,
                                   exemplars=rep(exs,length(partId)),
                                   outputMatrixAllSubjs[,exs])
                                   
# write csv in long format
# write.csv(outputMatrixAllSubjs, "exemplars_similarity/similarityMatrix.csv",row.names = F, na = "")




exs1to8 <- exs[1:8]
# create empty matrix for output
outputMatrix <- data.frame(matrix(NA, nrow = 8*8-8, ncol = 2+16))
colnames(outputMatrix) <- c("exA","exB",partId[order(partId)])
counter <- 0
for (i in 1:length(exs1to8)) {
  for (j in 1:length(exs1to8)) {
    filter <- ((allSubj$left == exs1to8[i] & allSubj$right == exs1to8[j]) | 
                 (allSubj$left == exs1to8[j] & allSubj$right == exs1to8[i]))
    
    if (any(filter)) {
      counter <- counter + 1
      parts <- allSubj[filter,] %>% group_by(partId) %>% #%>%
        summarize(rating=mean(rating))
      outputMatrix[counter,1:2] <- c(exs1to8[i],exs1to8[j])
      outputMatrix[counter,3:18] <- parts$rating
    }
  }
}

# Example data (matrix of 10 points in 3D space)
data <- outputMatrix[!is.na(outputMatrix$exA),3:18]
# Compute distance matrix
dist_matrix <- dist(t(data))
dist_matrix <- read.csv("exemplars_similarity/similarityMatrixAverage_task1.csv")
# dist_matrix <- read.csv("exemplars_similarity/similarityMatrixAverage_task2.csv")
dist_matrix <- dist_matrix[,-1]
dist_matrix <- as.dist(dist_matrix)

# Perform Classical MDS
mds_result <- cmdscale(dist_matrix, k = 2)  # k = number of dimensions
# euclidean distance
sqrt(sum((mds_result[1,] - mds_result[8,]) ^ 2))

# Plot the results
plot(mds_result, xlab = "Dimension 1", ylab = "Dimension 2", main = "Classical MDS")
text(mds_result, labels = 1:8, pos = 3)  # Add labels if needed



# read a csv that has been already averaged. This one contains the similariy matrices
# for all the subjects and only half of the off diagonal matrix
dist_matrix <- read.csv("exemplars_similarity/similarityMatrix_allPartOffDiagonals.csv")
partId <- unique(dist_matrix$partId)
for (i in 1:length(partId)) {
  temp <- dist_matrix[dist_matrix$partId == partId[i],]
  tempTask1 <- as.dist(temp[1:8,3:10])
  tempTask2 <- as.dist(temp[9:16,11:18])
  
  # Perform Classical MDS
  mds_result_t1 <- cmdscale(tempTask1, k = 2)  # k = number of dimensions
  mds_result_t2 <- cmdscale(tempTask2, k = 2)  # k = number of dimensions
  
  if (i == 1) {
    psychCoord1 <- data.frame(partId=partId[i],task="task1",
                             t(c(mds_result_t1)))
    psychCoord2 <- data.frame(partId=partId[i],task="task2",
                              t(c(mds_result_t2)))
  } else {
    psychCoord1 <- rbind(psychCoord1,data.frame(partId=partId[i],task="task1",
                              t(c(mds_result_t1))))
    psychCoord2 <- rbind(psychCoord2,data.frame(partId=partId[i],task="task2",
                              t(c(mds_result_t2))))
  }
}
# name columns adequately to their exemplars
colnames(psychCoord1)[-1:-2] <- c(paste0("x",1:8),paste0("y",1:8))
# colnames(psychCoord1)[-1:-2] <- c(paste0("x",1:8),paste0("y",1:8),paste0("z",1:8))
# colnames(psychCoord1)[-1:-2] <- c(paste0("x",1:8),paste0("y",1:8),paste0("z",1:8),paste0("w",1:8))
colnames(psychCoord2)[-1:-2] <- c(paste0("x",9:16),paste0("y",9:16))
# colnames(psychCoord2)[-1:-2] <- c(paste0("x",9:16),paste0("y",9:16),paste0("z",9:16))
# colnames(psychCoord2)[-1:-2] <- c(paste0("x",9:16),paste0("y",9:16),paste0("z",9:16),paste0("w",9:16))



# combination of numbers so we can compute the euclidean distance.
ex1 <- combinat::combn(1:8,2)
ex2 <- combinat::combn(9:16,2)
# distance matrices
dist1 <- matrix(NA, nrow=length(partId), ncol=ncol(ex1))
dist2 <- matrix(NA, nrow=length(partId), ncol=ncol(ex2))

for (i in 1:ncol(ex1)) {
  # task 1
  pc1_1 <- psychCoord1[,grepl(ex1[1,i],colnames(psychCoord1))]
  pc1_2 <- psychCoord1[,grepl(ex1[2,i],colnames(psychCoord1))]
  # euclidean distance
  dist1[,i] <- sqrt(rowSums((pc1_1[,] - pc1_2[,]) ^ 2))
  
  # task 2
  pc2_1 <- psychCoord2[,grepl(ex2[1,i],colnames(psychCoord2))]
  pc2_2 <- psychCoord2[,grepl(ex2[2,i],colnames(psychCoord2))]
  # euclidean distance
  dist2[,i] <- sqrt(rowSums((pc2_1[,] - pc2_2[,]) ^ 2))
}
colnames(dist1) <- paste0(ex1[1,],"_",ex1[2,])
colnames(dist2) <- paste0(ex2[1,],"_",ex2[2,])

# combine with psychological space
psychCoord1 <- cbind(psychCoord1, dist1)
psychCoord2 <- cbind(psychCoord2, dist2)



# # # # # Euclidean Distances for Test 1 # # # # #
lf_dist1 <- melt(psychCoord1, measure.vars = paste0(ex1[1,],"_",ex1[2,]))
lf_dist1$group <- recode(lf_dist1$variable, 
                         "1_2"="cat1","1_3"="cat1","1_4"="cat1","1_5"="between", 
                         "1_6"="between", "1_7"="between", "1_8"="between", 
                         "2_3"="cat1", "2_4"="cat1", "2_5"="between", 
                         "2_6"="between", "2_7"="between", "2_8"="between", 
                         "3_4"="cat1", "3_5"="between","3_6"="between",
                         "3_7"="between","3_8"="between","4_5"="between",
                         "4_6"="between","4_7"="between","4_8"="between",
                         "5_6"="cat2", "5_7"="cat2", "5_8"="cat2", "6_7"="cat2", 
                         "6_8"="cat2", "7_8"="cat2")
lf_dist1$cat_type <- ifelse(lf_dist1$group == "between","between","within")

anno <- data.frame(x1 = c(1), x2 = c(2), y1 = c(8.8), y2 = c(9), 
                   xstar = c(1.5), ystar = c(9.3), lab = c("ns"))

t.test(lf_dist1$value[lf_dist1$group=="cat1"],lf_dist1$value[lf_dist1$group=="cat2"])
shapiro.test(lf_dist1$value[lf_dist1$group=="cat1"])
ks.test(lf_dist1$value[lf_dist1$group=="cat1"],"pnorm")
wilcox.test(lf_dist1$value[lf_dist1$group=="cat1"],lf_dist1$value[lf_dist1$group=="cat2"])
p_task1_cat <- ggplot(lf_dist1[lf_dist1$cat_type=="within",], 
       aes(x=group,y=value)) + 
  labs(x = "Set 1 (A to H)", y="Euclidian Distance") +
  geom_violin(col = "grey80") + geom_boxplot(col = "grey20") + 
  geom_jitter(alpha=.2, width = .1, shape=16) + stat_summary() +
  geom_segment(data=anno, aes(x=x1,xend=x2,y=y2,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x1,xend=x1,y=y1,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x2,xend=x2,y=y1,yend=y2),inherit.aes=F) +
  geom_text(data=anno, aes(x = xstar,  y = ystar, label = lab), inherit.aes=F) +
  scale_y_continuous(breaks = seq(0,8,by=2), limits = c(0,9.5)) +
  scale_x_discrete(labels=c("A to D","E to H")) +
  theme_classic()

t.test(lf_dist1$value[lf_dist1$cat_type=="between"],lf_dist1$value[lf_dist1$cat_type!="between"])
shapiro.test(lf_dist1$value[lf_dist1$cat_type=="between"])
ks.test(lf_dist1$value[lf_dist1$cat_type=="between"],"pnorm",
        mean=mean(lf_dist1$value[lf_dist1$cat_type=="between"]),
        sd=sd(lf_dist1$value[lf_dist1$cat_type=="between"]))
wilcox.test(lf_dist1$value[lf_dist1$cat_type=="between"],lf_dist1$value[lf_dist1$cat_type!="between"])
p_task1_type <- ggplot(lf_dist1, aes(x=cat_type,y=value)) + 
  labs(x = "Set 1 (A to H)", y="Euclidian Distance") +
  geom_violin(col = "grey80") + geom_boxplot(col = "grey20") + 
  geom_jitter(alpha=.2, width = .1, shape=16) + stat_summary() +
  geom_segment(data=anno, aes(x=x1,xend=x2,y=y2,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x1,xend=x1,y=y1,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x2,xend=x2,y=y1,yend=y2),inherit.aes=F) +
  geom_text(data=anno, aes(x = xstar,  y = ystar, label = lab), inherit.aes=F) +
  scale_y_continuous(breaks = seq(0,8,by=2), limits = c(0,9.5)) +
  scale_x_discrete(labels=c("Between Cat.","Within Cat.")) +
  theme_classic()



# # # # # Euclidean Distances for Test 2 # # # # #
lf_dist2 <- melt(psychCoord2, measure.vars = paste0(ex2[1,],"_",ex2[2,]))
lf_dist2$group <- recode(lf_dist2$variable, 
                         "9_10"="cat1","9_11"="cat1","9_12"="cat1","9_13"="between", 
                         "9_14"="between", "9_15"="between", "9_16"="between", 
                         "10_11"="cat1", "10_12"="cat1", "10_13"="between", 
                         "10_14"="between", "10_15"="between", "10_16"="between", 
                         "11_12"="cat1", "11_13"="between","11_14"="between",
                         "11_15"="between","11_16"="between","12_13"="between",
                         "12_14"="between","12_15"="between","12_16"="between",
                         "13_14"="cat2", "13_15"="cat2", "13_16"="cat2", "14_15"="cat2", 
                         "14_16"="cat2", "15_16"="cat2")
lf_dist2$cat_type <- ifelse(lf_dist2$group == "between","between","within")

anno <- data.frame(x1 = c(1), x2 = c(2), y1 = c(8.8), y2 = c(9), 
                   xstar = c(1.5), ystar = c(9.3), lab = c("ns"))

t.test(lf_dist2$value[lf_dist2$group=="cat1"], lf_dist2$value[lf_dist2$group=="cat2"])
shapiro.test(lf_dist2$value[lf_dist2$group=="cat1"])
ks.test(lf_dist2$value[lf_dist2$group=="cat1"], "pnorm", 
        mean=mean(lf_dist2$value[lf_dist2$group=="cat1"]),
        sd=sd(lf_dist2$value[lf_dist2$group=="cat1"]))
wilcox.test(lf_dist2$value[lf_dist2$group=="cat1"], lf_dist2$value[lf_dist2$group=="cat2"])
p_task2_cat <- ggplot(lf_dist2[lf_dist2$cat_type=="within",], 
                      aes(x=group,y=value)) + 
  labs(x = "Set 2 (I to P)", y="Euclidian Distance") +
  geom_violin(col = "grey80") + geom_boxplot(col = "grey20") + 
  geom_jitter(alpha=.2, width = .1, shape=16) + stat_summary() +
  geom_segment(data=anno, aes(x=x1,xend=x2,y=y2,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x1,xend=x1,y=y1,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x2,xend=x2,y=y1,yend=y2),inherit.aes=F) +
  geom_text(data=anno, aes(x = xstar,  y = ystar, label = lab), inherit.aes=F) +
  scale_y_continuous(breaks = seq(0,8,by=2), limits = c(0,9.5)) +
  scale_x_discrete(labels=c("I to L", "M to P")) +
  theme_classic()

t.test(lf_dist2$value[lf_dist2$cat_type=="between"],lf_dist2$value[lf_dist2$cat_type!="between"])
shapiro.test(lf_dist2$value[lf_dist2$cat_type=="between"])
ks.test(lf_dist2$value[lf_dist2$cat_type=="between"],"pnorm",
        mean=mean(lf_dist2$value[lf_dist2$cat_type=="between"]),
        sd=sd(lf_dist2$value[lf_dist2$cat_type=="between"]))
wilcox.test(lf_dist2$value[lf_dist2$cat_type=="between"],lf_dist2$value[lf_dist2$cat_type!="between"])
p_task2_type <- ggplot(lf_dist2, aes(x=cat_type,y=value)) + 
  labs(x = "Set 2 (I to P)", y="Euclidian Distance") +
  geom_violin(col = "grey80") + geom_boxplot(col = "grey20") + 
  geom_jitter(alpha=.2, width = .1, shape=16) + stat_summary() +
  geom_segment(data=anno, aes(x=x1,xend=x2,y=y2,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x1,xend=x1,y=y1,yend=y2),inherit.aes=F) +
  geom_segment(data=anno, aes(x=x2,xend=x2,y=y1,yend=y2),inherit.aes=F) +
  geom_text(data=anno, aes(x = xstar,  y = ystar, label = lab), inherit.aes=F) +
  scale_y_continuous(breaks = seq(0,8,by=2), limits = c(0,9.5)) +
  scale_x_discrete(labels=c("Between Cat.","Within Cat.")) +
  theme_classic()

plot <- annotate_figure(ggarrange(p_task1_cat,p_task2_cat,
                                  p_task1_type,p_task2_type, labels=LETTERS[1:4]),
                        top = text_grob("Distance in Perceptual Space (k=2)", color="black", face="bold", size=14))

# combine
print_fig <- 1
if (print_fig == 1) {
  ggsave("analysis/figure3.pdf", scale=1,
         plot = plot, width = 15, height = 15, units = "cm", dpi = 900, 
         limitsize = T)
}



# MDS for each participant
# distance between exemplars 
# calculate differences between and within categories
# how many dimensions (elbow method)?