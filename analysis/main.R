# get files cotaning ".csv" from data folder
files <- list.files("data/",pattern =  ".csv")

# create vector with relevant columns to filter read csvs
rel_cols <- c("participant","task","phase","trials.thisRepN","response.corr")

for (i in 1:length(files)) {
  temp <- read.csv(paste0("data/",files[i]))

  temp <- temp[!is.na(temp$phase),rel_cols]
    
  if (i == 1) {
    lf <- temp
  } else {
    lf <- rbind(lf,temp)
  }
}
lf$trials.thisRepN <- lf$trials.thisRepN + 1

library(ggplot2)
ggplot(lf, aes(x=trials.thisRepN,y=response.corr,col=task)) +
  labs(x="blocks", y="p(correct)") +
  geom_hline(yintercept = 0.5) +
  stat_summary(geom="line") +
  stat_summary(geom="errorbar") +
  facet_grid(participant ~ phase) +
  theme_bw()
