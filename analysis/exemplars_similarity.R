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
  # remove rows with no response
  oneSubj <- oneSubj[!is.na(oneSubj$slider.response),]
  # combine
  if (i == 1) {
    allSubj <- data.frame(oneSubj,partId=paste0("parID_",i)) 
  } else {
    allSubj <- rbind(allSubj,data.frame(oneSubj,partId=paste0("parID_",i)) )
  }
}
# left and right as ordered factors
allSubj$left <- factor(allSubj$left, levels = exs) 
allSubj$right <- factor(allSubj$right, levels = exs) 
table(allSubj$left,allSubj$right)


library(ggplot2)
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

# write csv in long format
write.csv(allSubj, "exemplars_similarity/longFormat.csv",row.names = F)

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
write.csv(outputMatrix, "exemplars_similarity/similarityMatrixAverage.csv",row.names = F)



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
write.csv(outputMatrixAllSubjs, "exemplars_similarity/similarityMatrix.csv",row.names = F, na = "")




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
data <- outputMatrix[,3:18]
# Compute distance matrix
dist_matrix <- dist(t(data))

# Perform Classical MDS
mds_result <- cmdscale(dist_matrix, k = 2)  # k = number of dimensions
# Plot the results
plot(mds_result, xlab = "Dimension 1", ylab = "Dimension 2", main = "Classical MDS")
text(mds_result, labels = 1:10, pos = 3)  # Add labels if needed
