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
if (!require(cowplot)) {install.packages("cowplot")}; library(cowplot)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)

# visualize all subjects

# correctness
# pEasyCorr <- ggplot(lfe, aes(x=blocks,y=response.corr,col=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black", alpha=0.5) +
#   geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
#   scale_x_continuous(breaks = seq(0,12,by=2)) +
#   stat_summary(geom="line",position = position_dodge(0.2)) +
#   stat_summary(geom="errorbar",position = position_dodge(0.2)) +
#   facet_wrap(subjectId ~ .) +
#   theme_bw()
# pHardCorr <- ggplot(lfh, aes(x=blocks,y=response.corr,col=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition") +
#   geom_vline(xintercept = 8.5, col="black") +
#   geom_hline(yintercept = 0.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=2)) +
#   stat_summary(geom="line",position = position_dodge(0.2)) +
#   stat_summary(geom="errorbar",position = position_dodge(0.2)) +
#   facet_wrap(subjectId ~ .) +
#   theme_bw()

# reaction time
# pEasyRT <- ggplot(lfe[lfe$goodTrials==T,], aes(x=blocks,y=response.rt,col=condition2,shape=task)) +
#   labs(x="blocks", y="RT (sec.)",col="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=2)) +
#   stat_summary(geom="line",fun = median,position = position_dodge(0.2)) +
#   # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
#   facet_wrap(subjectId ~ .) +
#   theme_bw()
# pHardRT <- ggplot(lfh[lfh$goodTrials==T,], aes(x=blocks,y=response.rt,col=condition2,shape=task)) +
#   labs(x="blocks", y="RT (sec.)",col="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=2)) +
#   stat_summary(geom="line",fun = median,position = position_dodge(0.2)) +
#   # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
#   facet_wrap(subjectId ~ .) +
#   theme_bw()

# remove bad trials 
lfe <- lfe[lfe$goodTrials==T,]
lfh <- lfh[lfh$goodTrials==T,]

# remove bad participants 
lfe <- lfe[lfe$goodSubject==T,]
lfh <- lfh[lfh$goodSubject==T,]



# clean data to visualize means with standard erros using the The Cousineau–Morey method

# experiment 1a
grand_mean_corr <- mean(lfe$response.corr)
grand_mean_rt <- mean(lfe$response.rt)
# averages per participant, condition, and blocks 
tmp <- lfe %>% group_by(participant,task,phase,blocksPhase,blocks,condition,condition2) %>%
  summarise(corr = mean(response.corr),
            rt = mean(response.rt)) %>% ungroup()
# scale averages
tmp <- tmp %>% group_by(participant,task,phase,condition,condition2) %>%
  mutate(corr_id = mean(corr),
         rt_id = mean(rt),
         scale_corr = corr-corr_id+grand_mean_corr,
         scale_rt = rt-rt_id+grand_mean_rt) %>% ungroup()
# estimate means and standard errors
lfe_plot <- tmp %>% group_by(task,phase,blocksPhase,blocks,condition,condition2) %>%
  summarise(n=n(),
            m_corr = mean(corr),
            m_rt = mean(rt),
            m_scale_corr = mean(scale_corr),
            m_scale_rt = mean(scale_rt),
            sd_scale_corr = sd(scale_corr),
            sd_scale_rt = sd(scale_rt),
            sem_scale_corr = sd_scale_corr/sqrt(n)*sqrt(36/35),
            sem_scale_rt = sd_scale_rt/sqrt(n)*sqrt(36/35),
            error_min_corr = m_corr-sem_scale_corr,
            error_max_corr = m_corr+sem_scale_corr,
            error_min_rt = m_rt-sem_scale_rt,
            error_max_rt = m_rt+sem_scale_rt) %>% ungroup()
lfe_plot <- rbind(lfe_plot, 
                  lfe_plot %>% filter(blocks <= 6) %>%
                    filter(blocks == 6) %>% # Get the last point before the break
                    mutate(
                      blocks = 6.5, # Place the NA at the exact x-intercept of the vline
                      m_corr = NA,  # Set y-value to NA for the break
                      error_min_corr = NA, # Also set error bars to NA to avoid plotting
                      error_max_corr = NA,
                      m_rt = NA,  # Set y-value to NA for the break
                      error_min_rt = NA, # Also set error bars to NA to avoid plotting
                      error_max_rt = NA
                    )
)

# experiment 1b
grand_mean_corr <- mean(lfh$response.corr)
grand_mean_rt <- mean(lfh$response.rt)
# averages per participant, condition, and blocks 
tmp <- lfh %>% group_by(participant,task,phase,blocksPhase,blocks,condition,condition2) %>%
  summarise(corr = mean(response.corr),
            rt = mean(response.rt)) %>% ungroup()
# scale averages
tmp <- tmp %>% group_by(participant,task,phase,condition,condition2) %>%
  mutate(corr_id = mean(corr),
         rt_id = mean(rt),
         scale_corr = corr-corr_id+grand_mean_corr,
         scale_rt = rt-rt_id+grand_mean_rt) %>% ungroup()
# estimate means and standard errors
lfh_plot <- tmp %>% group_by(task,phase,blocksPhase,blocks,condition,condition2) %>%
  summarise(n=n(),
            m_corr = mean(corr),
            m_rt = mean(rt),
            m_scale_corr = mean(scale_corr),
            m_scale_rt = mean(scale_rt),
            sd_scale_corr = sd(scale_corr),
            sd_scale_rt = sd(scale_rt),
            sem_scale_corr = sd_scale_corr/sqrt(n)*sqrt(36/35),
            sem_scale_rt = sd_scale_rt/sqrt(n)*sqrt(36/35),
            error_min_corr = m_corr-sem_scale_corr,
            error_max_corr = m_corr+sem_scale_corr,
            error_min_rt = m_rt-sem_scale_rt,
            error_max_rt = m_rt+sem_scale_rt) %>% ungroup()
lfh_plot <- rbind(lfh_plot, 
                  lfh_plot %>% filter(blocks <= 8) %>%
                    filter(blocks == 8) %>% # Get the last point before the break
                    mutate(
                      blocks = 8.5, # Place the NA at the exact x-intercept of the vline
                      m_corr = NA,  # Set y-value to NA for the break
                      error_min_corr = NA, # Also set error bars to NA to avoid plotting
                      error_max_corr = NA,
                      m_rt = NA,  # Set y-value to NA for the break
                      error_min_rt = NA, # Also set error bars to NA to avoid plotting
                      error_max_rt = NA
                      )
                  )






# visualize average
pos <- .4#.3
stroke_size <- .7

# y axis limits
min_corr_y <- max(0,min(c(lfh_plot$error_min_corr,lfe_plot$error_min_corr),na.rm=T))
max_corr_y <- min(1,max(c(lfh_plot$error_max_corr,lfe_plot$error_max_corr),na.rm=T))
min_corr_y <- round(min_corr_y,2)
max_corr_y <- round(max_corr_y,2)

min_rt_y <- min(c(lfh_plot$error_min_rt,lfe_plot$error_min_rt),na.rm=T)
max_rt_y <- max(c(lfh_plot$error_max_rt,lfe_plot$error_max_rt),na.rm=T)
min_rt_y <- round(min_rt_y,2)
max_rt_y <- round(max_rt_y,2)



# correctness
# figA <- ggplot(lfe[lfe$goodTrials==T,], aes(x=blocks,y=response.corr,shape=condition2,
#                                             col=condition2,linetype=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black", alpha=0.5) +
#   geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.25,.5,.85)) +
#   coord_cartesian(ylim = c(0.25,0.9)) +
#   # scale_color_manual(values = c("red","blue","green")) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = c(0.25,0.2),
#                      legend.background = element_blank(),
#                      legend.title = element_blank())

figA <- ggplot(lfe_plot, aes(x=blocks,y=m_corr,shape=condition2,
                             col=condition2,linetype=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black", alpha=0.5) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_corr_y,.5, max_corr_y)) +
  coord_cartesian(ylim = c(min_corr_y, max_corr_y)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() +  theme(legend.position = "none")
# figD <- ggplot(lfh[lfh$goodTrials==T,], aes(x=blocks,y=response.corr,shape=condition2,
#                                             col=condition2,linetype=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_vline(xintercept = 8.5, col="black") +
#   geom_hline(yintercept = 0.5) +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.25,.5,.85)) +
#   coord_cartesian(ylim = c(0.25,0.9)) +
#   # scale_color_manual(values = c("red","blue","green")) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
figD <- ggplot(lfh_plot, aes(x=blocks,y=m_corr,shape=condition2,
                             col=condition2,linetype=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black", alpha=0.5) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_corr_y,.5, max_corr_y)) +
  coord_cartesian(ylim = c(min_corr_y, max_corr_y)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none")



# temp <- lfe[lfe$phase==2 & lfe$blocksPhase <= 2 & lfe$condition2!="partial-reversed",]
# temp <- lfe[lfe$phase==2 & lfe$blocksPhase <= 2,]
# insetA <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2, linetype=condition2,
#                            shape=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
#   scale_x_continuous(breaks = c(7, 8)) +
#   scale_y_continuous(breaks = c(.25, .5, .8)) +
#   coord_cartesian(ylim = c(0.25,0.85), xlim = c(6.6,8.4)) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.5) +
#   stat_summary(geom="errorbar",width=.5,position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=1.8,fill="white") +
#   theme_bw() + theme(legend.position = "none",
#                      plot.margin = unit(c(0, 0, 0, 0), "cm"),
#                      axis.title = element_blank(),
#                      axis.text = element_text(size = 6),
#                      plot.background = element_rect(color = "black", linewidth = .4))

temp <- lfe_plot[lfe_plot$phase==2 & lfe_plot$blocksPhase <= 2,]
insetA <- ggplot(temp, aes(x=blocks,y=m_corr,col=condition2, linetype=condition2,
                           shape=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = c(7, 8)) +
  scale_y_continuous(breaks = c(.25, .5, .8)) +
  coord_cartesian(ylim = c(0.25,0.85), xlim = c(6.6,8.4)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  # geom_errorbar(position = position_dodge(pos), size=.4, width=.2,
  #               aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=1.7, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none",
                     plot.margin = unit(c(0, 0, 0, 0), "cm"),
                     axis.title = element_blank(),
                     axis.text = element_text(size = 6),
                     plot.background = element_rect(color = "black", linewidth = .4))

# temp <- lfh[lfh$phase==2 & lfh$blocksPhase <= 2 & lfh$condition2!="partial-reversed",]
# temp <- lfh[lfh$phase==2 & lfh$blocksPhase <= 2,]
# insetD <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2, linetype=condition2,
#                            shape=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
#   scale_x_continuous(breaks = c(9, 10)) +
#   scale_y_continuous(breaks = c(.35, .5, .7)) +
#   coord_cartesian(ylim = c(.35, .72), xlim = c(8.6,10.4)) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.5) +
#   stat_summary(geom="errorbar",width=.5,position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=1.8,fill="white") +
#   theme_bw() + theme(legend.position = "none",
#                      plot.margin = unit(c(0, 0, 0, 0), "cm"),
#                      axis.title = element_blank(),
#                      axis.text = element_text(size = 6),
#                      plot.background = element_rect(color = "black", linewidth = .4))
temp <- lfh_plot[lfh_plot$phase==2 & lfh_plot$blocksPhase <= 2,]
insetD <- ggplot(temp, aes(x=blocks,y=m_corr,col=condition2, linetype=condition2,
                           shape=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  scale_x_continuous(breaks = c(9, 10)) +
  scale_y_continuous(breaks = c(.25, .5, .8)) +
  coord_cartesian(ylim = c(0.25,0.85), xlim = c(8.6,10.4)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  # geom_errorbar(position = position_dodge(pos), size=.4, width=.2,
  #               aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=1.7, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none",
                     plot.margin = unit(c(0, 0, 0, 0), "cm"),
                     axis.title = element_blank(),
                     axis.text = element_text(size = 6),
                     plot.background = element_rect(color = "black", linewidth = .4))



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



# # # # # # Phase 1: # # # # # #
# # # Total Reversed versus Partial Reversed # # #
temp <- lfe[lfe$phase==1 & lfe$condition!="nonreversed",]
# temp <- lfe[lfe$phase==1 & lfe$condition2!="partial-nonreversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 1 and 2: # # # # # #
temp <- lfe[(lfe$blocks==6|lfe$blocks==7) & lfe$condition2!="partial-nonreversed",]
m <- glmer(response.corr~as.factor(blocks)+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 2: # # # # # #
# # # Partial Reversed versus Partial Nonreversed # # #
# first two blocks (Generalization of error)
temp <- lfe[lfe$phase==2 & lfe$blocksPhase <= 2 & lfe$condition2!="total-reversed",]
m <- glmer(response.corr~blocksPhase*condition2+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)

# block 1 in phase 2
temp <- lfe[lfe$phase==2 & lfe$blocks == 7 & lfe$condition2!="total-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)
# block 2 in phase 2
temp <- lfe[lfe$phase==2 & lfe$blocks == 8 & lfe$condition2!="total-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)


# # # Total Reversed versus Partial Nonreversed # # #
temp <- lfe[lfe$phase==2 &  lfe$blocksPhase <= 2 & lfe$condition2!="partial-reversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)

# block 1 in phase 2
temp <- lfe[lfe$phase==2 & lfe$blocks == 7 & lfe$condition2!="partial-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)
# block 2 in phase 2
temp <- lfe[lfe$phase==2 & lfe$blocks == 8 & lfe$condition2!="partial-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)



# # # Total Reversed versus Partial Reversed # # #
# temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]
# figC <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2,
#                          shape=condition2,linetype=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black") +
#   geom_hline(yintercept = 0.5) +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.25,.5,.85)) +
#   coord_cartesian(ylim = c(0.25,0.85)) +
#   # scale_color_manual(values = c("red","blue")) +
#   scale_color_manual(values = c("blue","orange")) +
#   scale_linetype_manual(values = c("solid","solid")) +
#   scale_shape_manual(values = c(21,22)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos), width=.3) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
# figC <- ggdraw(figC) + draw_plot(insetA,
#                                  x = 0.77,    # x-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
#                                  y = 0.17,    # y-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
#                                  width = 0.2, # Width of the inset (0 to 1, relative to main plot)
#                                  height = 0.3 # Height of the inset (0 to 1, relative to main plot)
# )
temp <- lfe_plot[lfe_plot$phase==2 & lfe_plot$condition!="nonreversed",]
figC <- ggplot(temp, aes(x=blocks,y=m_corr,col=condition2,
                         shape=condition2,linetype=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_corr_y,.5,max_corr_y)) +
  coord_cartesian(ylim = c(min_corr_y,max_corr_y)) +
  scale_color_manual(values = c("blue","orange")) +
  scale_linetype_manual(values = c("solid","solid")) +
  scale_shape_manual(values = c(21,22)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none")
figC <- ggdraw(figC) + draw_plot(insetA,
                                 x = 0.82,    # x-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
                                 y = 0.175,    # y-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
                                 width = 0.15, # Width of the inset (0 to 1, relative to main plot)
                                 height = 0.275 # Height of the inset (0 to 1, relative to main plot)
)



# all blocks
temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m); report::report_table(m)
# what about reaction time
m <- lmer(response.rt~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))


m <- lmer(log(response.rt)~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))

m <- lmer(sqrt(response.rt)~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))






# # # # # # Phase 1: # # # # # #
# # # Total Reversed versus Partial Reversed # # #
temp <- lfh[lfh$phase==1 & lfh$condition!="nonreversed",]
# temp <- lfh[lfh$phase==1 & lfh$condition2!="partial-nonreversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 1 and 2: # # # # # #
temp <- lfh[(lfh$blocks==8|lfh$blocks==9) & lfh$condition2!="partial-nonreversed",]
m <- glmer(response.corr~as.factor(blocks)+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 2: # # # # # #
# # # Partial Reversed versus Partial Nonreversed # # #
# first two blocks (Generalization of error)
temp <- lfh[lfh$phase==2 & lfh$blocksPhase <= 2 & lfh$condition2!="total-reversed",]
m <- glmer(response.corr~blocksPhase*condition2+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)

# block 1 in phase 2
temp <- lfh[lfh$phase==2 & lfh$blocks == 9 & lfh$condition2!="total-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)
# block 2 in phase 2
temp <- lfh[lfh$phase==2 & lfh$blocks == 10 & lfh$condition2!="total-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)


# # # Total Reversed versus Partial Nonreversed # # #
temp <- lfh[lfh$phase==2 &  lfh$blocksPhase <= 2 & lfh$condition2!="partial-reversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)

# block 1 in phase 2
temp <- lfh[lfh$phase==2 & lfh$blocks == 9 & lfh$condition2!="partial-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)
# block 2 in phase 2
temp <- lfh[lfh$phase==2 & lfh$blocks == 10 & lfh$condition2!="partial-reversed",]
m <- glmer(response.corr~condition2+(1|participant),family = binomial,temp)
summary(m);report::report_table(m)



# # # Total Reversed versus Partial Reversed # # #
# temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]
# figF <- ggplot(temp, aes(x=blocks,y=response.corr,col=condition2,
#                          shape=condition2,linetype=condition2)) +
#   labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
#        linetype="Task and Condition") +
#   geom_vline(xintercept = 8.5, col="black") +
#   geom_hline(yintercept = 0.5) +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.25,.5,.85)) +
#   coord_cartesian(ylim = c(0.25,0.85)) +
#   # scale_color_manual(values = c("red","blue")) +
#   scale_color_manual(values = c("blue","orange")) +
#   scale_linetype_manual(values = c("solid","solid")) +
#   scale_shape_manual(values = c(21,22)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos), width=.3) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
# figF <- ggdraw(figF) + draw_plot(insetD,
#                                  x = .77,    # x-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
#                                  y = .17,    # y-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
#                                  width = 0.2, # Width of the inset (0 to 1, relative to main plot)
#                                  height = 0.3 # Height of the inset (0 to 1, relative to main plot)
# )
temp <- lfh_plot[lfh_plot$phase==2 & lfh_plot$condition!="nonreversed",]
figF <- ggplot(temp, aes(x=blocks,y=m_corr,col=condition2,
                         shape=condition2,linetype=condition2)) +
  labs(x="blocks", y="p(correct)",col="Task and Condition",shape="Task and Condition",
       linetype="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_corr_y,.5,max_corr_y)) +
  coord_cartesian(ylim = c(min_corr_y,max_corr_y)) +
  scale_color_manual(values = c("blue","orange")) +
  scale_linetype_manual(values = c("solid","solid")) +
  scale_shape_manual(values = c(21,22)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_corr, ymax=error_max_corr), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none")
figF <- ggdraw(figF) + draw_plot(insetD,
                                 x = 0.82,    # x-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
                                 y = 0.175,    # y-coordinate of the bottom-left corner of the inset (0 to 1, relative to main plot)
                                 width = 0.15, # Width of the inset (0 to 1, relative to main plot)
                                 height = 0.275 # Height of the inset (0 to 1, relative to main plot)
)



# all blocks
temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m) ;report::report_table(m)
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m); report::report_table(m)
# what about reaction time
m <- lmer(response.rt~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))


m <- lmer(log(response.rt)~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))

m <- lmer(sqrt(response.rt)~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))





# participant vector for easy experiment
participantsE <- unique(lfe$participant)

# collapsed trials to get median RT for the easy experiment
collapsedE <- as.data.frame(lfe[lfe$goodSubject==T & lfe$goodTrials==T,] %>% 
                             group_by(participant,subjectId,first,task,difficulty,
                                      set,phase,condition2,blocks,blocksPhase) %>%
                             summarize(nTrials=n(),
                                       pCorrect=mean(response.corr,na.rm=T),
                                       avgRt=mean(response.rt,na.rm=T),
                                       medRt=median(response.rt,na.rm=T)))
temp <- collapsedE[collapsedE$phase==2&collapsedE$condition2!="partial-nonreversed",]
m <- lmer(medRt~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m)

# figure B
# figB <- ggplot(collapsedE, aes(x=blocks,y=medRt,col=condition2,
#                                shape=condition2,linetype=condition2)) +
# # figB <- ggplot(lfe, aes(x=blocks,y=response.rt,col=condition2,
# #                         shape=condition2,linetype=condition2)) +
#   labs(x="blocks", y="Mean of Medians RT (sec.)",shape="Task and Condition",
#        col="Task and Condition",linetype="Task and Condition") +
#   geom_vline(xintercept = 6.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.6,1,1.25)) +
#   coord_cartesian(ylim = c(0.6,1.25)) +
#   # scale_color_manual(values = c("red","blue","green")) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
#   # theme(legend.position = c(0.75,0.8),
#   #                    legend.background = element_blank(),
#   #                    legend.title = element_blank())
figB <- ggplot(lfe_plot, aes(x=blocks,y=m_rt,col=condition2,
                               shape=condition2,linetype=condition2)) +
  labs(x="blocks", y="Average RT (sec.)",shape="Task and Condition",
       col="Task and Condition",linetype="Task and Condition") +
  geom_vline(xintercept = 6.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_rt_y,1,max_rt_y)) +
  coord_cartesian(ylim = c(min_rt_y,max_rt_y)) +
  # scale_color_manual(values = c("red","blue","green")) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_rt, ymax=error_max_rt), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = c(0.75,0.8),
                     legend.background = element_blank(),
                     legend.title = element_blank())


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
temp <- collapsedH[collapsedH$phase==2&collapsedH$condition2!="partial-nonreversed",]
m <- lmer(medRt~blocksPhase*task+(blocksPhase|participant),REML=F,temp)
summary(m)

# figure E
# figE <- ggplot(collapsedH, aes(x=blocks,y=medRt,col=condition2,
#                                shape=condition2,linetype=condition2)) +
# # figE <- ggplot(lfh, aes(x=blocks,y=response.rt,col=condition2,
# #                         shape=condition2,linetype=condition2)) +
#   labs(x="blocks", y="Mean of Medians RT (sec.)",shape="Task and Condition",
#        col="Task and Condition",linetype="Task and Condition") +
#   geom_vline(xintercept = 8.5, col="black") +
#   scale_x_continuous(breaks = seq(0,12,by=1)) +
#   scale_y_continuous(breaks = c(.6,1,1.25)) +
#   coord_cartesian(ylim = c(0.6,1.25)) +
#   # scale_color_manual(values = c("red","blue","green")) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   scale_shape_manual(values = c(21,22,17)) +
#   stat_summary(geom="line",position = position_dodge(pos),size=.7) +
#   stat_summary(geom="errorbar",position = position_dodge(pos)) +
#   stat_summary(geom="point",position = position_dodge(pos),size=2.3,fill="white") +
#   # facet_grid(. ~ first) +
#   theme_bw() + theme(legend.position = "none")
figE <- ggplot(lfh_plot, aes(x=blocks,y=m_rt,col=condition2,
                             shape=condition2,linetype=condition2)) +
  labs(x="blocks", y="Average RT (sec.)",shape="Task and Condition",
       col="Task and Condition",linetype="Task and Condition") +
  geom_vline(xintercept = 8.5, col="black") +
  scale_x_continuous(breaks = seq(0,12,by=1)) +
  scale_y_continuous(breaks = c(min_rt_y,1,max_rt_y)) +
  coord_cartesian(ylim = c(min_rt_y,max_rt_y)) +
  # scale_color_manual(values = c("red","blue","green")) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  scale_shape_manual(values = c(21,22,17)) +
  geom_line(position = position_dodge(pos)) +
  geom_errorbar(position = position_dodge(pos), size=.4, width=.3, linewidth=.3, 
                aes(ymin=error_min_rt, ymax=error_max_rt), linetype="solid") +
  geom_point(position = position_dodge(pos), fill="white", size=2.3, stroke = stroke_size) + 
  theme_bw() + theme(legend.position = "none")




left <- annotate_figure(ggarrange(figA,figB,figC,nrow=3,
                                  labels = c("A","B","C")),
                        top = text_grob("Experiment 1a", color = "black", face = "bold", size = 14))
right <- annotate_figure(ggarrange(figD,figE,figF,nrow=3,
                                   labels = c("D","E","F")),
                         top = text_grob("Experiment 1b", color = "black", face = "bold", size = 14))

figure2 <- ggarrange(left,right,ncol=2,common.legend = T)
figure2


# combine
print_fig <- 1
if (print_fig == 1) {
    ggsave("analysis/figure2_v4.pdf", scale=.9,
         plot = figure2, width = 24, height = 24, units = "cm", dpi = 900, 
         limitsize = T)
}





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
