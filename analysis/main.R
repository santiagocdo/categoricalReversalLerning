# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# libraries to data managing and visualizations
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(blme)) {install.packages('blme')}; library(blme)
if (!require(emmeans)) {install.packages("emmeans")}; library(emmeans)
if (!require(cowplot)) {install.packages("cowplot")}; library(cowplot)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(bayestestR)) {install.packages("bayestestR")}; library(bayestestR)


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

# remove bad trials 
lfe <- lfe[lfe$goodTrials==T,]
lfh <- lfh[lfh$goodTrials==T,]

# remove bad participants 
lfe <- lfe[lfe$goodSubject==T,]
lfh <- lfh[lfh$goodSubject==T,]

# transform blocks for stats (so intercept make sense (block 1 = 0)
lfe$blocksStats <- lfe$blocksPhase - 1
lfh$blocksStats <- lfh$blocksPhase - 1

# add 1st and 2nd task order
lfe$task_order <- paste0(lfe$first,lfe$task)
lfe$task_order <- recode(lfe$task_order,
                         "first-partialpartial"="1st task","first-partialtotal"="2nd task",
                         "first-totalpartial"="2nd task","first-totaltotal"="1st task")
lfh$task_order <- paste0(lfh$first,lfh$task)
lfh$task_order <- recode(lfh$task_order,
                         "first-partialpartial"="1st task","first-partialtotal"="2nd task",
                         "first-totalpartial"="2nd task","first-totaltotal"="1st task")


# # # # # # The Cousineau–Morey method # # # # # ####
# clean data to visualize means with standard errors using the The Cousineau–Morey method

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



# # # # # # # visualize average # # # # # ####
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



# # # correctness # # #

# figure A
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

# figure D
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

# Figure inset A
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

# Figure inset D
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



# # reviewer 2's response with respect propositional versus associative (meta-learning in phase2)
# ggarrange(
#   ggplot(lfe[lfe$phase==2,], aes(x=blocksStats, y=response.corr, col=condition2, linetype=condition2, shape=condition2)) + 
#     labs(title = "Experiment 1",y="p(correct)",x="Blocks-1 in Reversal Phase",col="Task and Condition",
#          shape="Task and Condition",linetype="Task and Condition") +
#     stat_summary(geom="line",position = position_dodge(.5)) + stat_summary(fill="white",size=.6, position = position_dodge(.5)) + 
#     scale_color_manual(values = c("blue","orange","orange")) +
#     scale_linetype_manual(values = c("solid","solid","dotted")) +
#     scale_shape_manual(values = c(21,22,17)) + facet_grid(.~task_order) + theme_bw() + theme(legend.position = c(.82,.24)),
#   ggplot(lfh[lfh$phase==2,], aes(x=blocksStats, y=response.corr, col=condition2, linetype=condition2, shape=condition2)) + 
#     labs(title = "Experiment 2",y="p(correct)",x="Blocks-1 in Reversal Phase",col="Task and Condition",
#          shape="Task and Condition",linetype="Task and Condition") +
#     stat_summary(geom="line",position = position_dodge(.5)) + stat_summary(fill="white",size=.6, position = position_dodge(.5)) + 
#     scale_color_manual(values = c("blue","orange","orange")) +
#     scale_linetype_manual(values = c("solid","solid","dotted")) +
#     scale_shape_manual(values = c(21,22,17)) + facet_grid(.~task_order) + theme_bw() + theme(legend.position = c(.82,.24)), 
#   nrow=2, common.legend = T
# )
# more precisely (this was included in the reviewers' response and supplementary information)
ggplot(lfe[lfe$phase==2 & lfe$condition!="nonreversed" & lfe$blocksStats==0,], 
       aes(x=task_order, y=response.corr, col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~task_order*task+(1|participant), family = binomial, 
              lfe[lfe$phase==2 & lfe$condition!="nonreversed" & lfe$blocksStats==0,]))
summary(glmer(response.corr~task_order+task+(1|participant), family = binomial, 
              lfe[lfe$phase==2 & lfe$condition!="nonreversed" & lfe$blocksStats==0,]))
ggplot(lfh[lfh$phase==2 & lfh$condition!="nonreversed" & lfh$blocksStats==0,], 
       aes(x=task_order, y=response.corr, col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~task_order*task+(1|participant), family = binomial, 
              lfh[lfh$phase==2 & lfh$condition!="nonreversed" & lfh$blocksStats==0,]))
summary(glmer(response.corr~task_order+task+(1|participant), family = binomial, 
              lfh[lfh$phase==2 & lfh$condition!="nonreversed" & lfh$blocksStats==0,]))



# # # # # # EXPERIMENT 1 # # # # # ####
# # # Total Reversed versus Partial Reversed # # #

# all blocks
temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]

# model adjusted by task_order 
summary(glmer(response.corr~blocksStats*task+task_order+(blocksStats|participant), family = binomial, temp))
# analysis per task order
ggplot(temp, aes(x=blocksStats, y=response.corr, col=condition2)) + facet_grid(.~task_order) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$task_order=="1st task",]))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$task_order=="2nd task",]))
# colSums(table(temp$participant,temp$first)!=0)

# Reviewer 2 remove first trial sensitivity analysis
ggplot(temp[temp$ex_counter!=min(temp$ex_counter),],aes(x=ex_counter,y=response.corr,col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$ex_counter!=min(temp$ex_counter),]))
# Reviewer 2 analysis of first (pre-feedback) and second (post-feedback) responses 
ggplot(temp[temp$ex_counter<=min(temp$ex_counter)+1,],aes(x=ex_counter,y=response.corr,col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~as.factor(ex_counter)*task+(1|participant), family = binomial, temp[temp$ex_counter<=min(temp$ex_counter)+1,]))

# main analysis
m0 <- glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp)
# if singular fit, then try bayesian. Use blme instead of lme4 if glmer is singular=TRUE
# m <- bglmer(response.corr~blocksStats*task+(blocksStats|participant), control = glmerControl(optimizer = "bobyqa"), family = binomial,temp)
summary(m0); report::report_table(m0)
# no significant interaction, then (Gelman & Hill, 2006)
m <- glmer(response.corr~blocksStats+task+(blocksStats|participant), family = binomial, temp)
anova(m0, m)
summary(m); report::report_table(m)
summary(emmeans(m, pairwise ~ task | blocksStats,type = "response"))

# 1. Create a data frame for predictions. We use re.form = NA to get population-level effects (ignoring random intercepts)
temp$pred <- predict(m, newdata = temp, type = "response", re.form = NA)
# 2. Aggregate predictions by blocks (to match the geom_line structure). If your plot separates by condition, aggregate by both.
model_preds <- aggregate(pred ~ blocks + condition2, data = temp, FUN = mean)



# visualization
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
  # geom_line(position = position_dodge(pos)) +
  geom_line(data = model_preds, aes(x = blocks, y = pred, group = condition2),
            position = position_dodge(pos)) +
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
temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m); report::report_table(m)



# # # # # # Phase 1 and 2: # # # # # #
temp <- lfe[(lfe$blocks==6|lfe$blocks==7) & lfe$condition2!="partial-nonreversed",]
m <- glmer(response.corr~as.factor(blocks)+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 2: # # # # # #
# # # Partial Reversed versus Partial Nonreversed # # #
# first two blocks (Generalization of error)
temp <- lfe[lfe$phase==2 & lfe$blocks <= 8 & lfe$condition2!="total-reversed",]
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



# # # # # # Phase 2: Reaction Time # # # # # #
# what about reaction time
temp <- lfe[lfe$phase==2 & lfe$condition!="nonreversed",]
m <- lmer(response.rt~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
# no significant interaction, then (Gelman & Hill, 2006)
m <- lmer(response.rt~blocksStats+task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
m0 <- lmer(response.rt~blocksStats+(blocksStats|participant),REML=F,temp)
anova(m,m0); bayesfactor_models(m0, denominator = m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))
qqnorm(resid(m)); qqline(resid(m), col = "red")

m <- lmer(log(response.rt)~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))
qqnorm(resid(m)); qqline(resid(m), col = "red")

m <- lmer(sqrt(response.rt)~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))
qqnorm(resid(m)); qqline(resid(m), col = "red")

m <- glmer(response.rt~blocksStats*task+(blocksStats|participant),famil=Gamma(link = "log"),temp)
summary(m); report::report_table(m)



# # # # # # Phase 1: Accuracy # # # # # #
# # # Total Reversed versus Partial Reversed # # #
temp <- lfe[lfe$phase==1 & lfe$condition!="nonreversed",]
# temp <- lfe[lfe$phase==1 & lfe$condition2!="partial-nonreversed",]
m <- glmer(response.corr~blocksStats*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)
# no significant interaction, then (Gelman & Hill, 2006)
m <- glmer(response.corr~blocksStats+task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)
m0 <- glmer(response.corr~blocksStats+(blocksStats|participant),family = binomial,temp)
anova(m,m0); bayesfactor_models(m0, denominator = m)



# reviewer 2, previous to the last comment
# temp0 <- as.data.frame(lfh[lfh$phase==2 & lfh$blocksStats == 0,] %>%
#   group_by(participant, task, r_correct) %>%
#   mutate(cat_count = row_number()) %>%
#   ungroup())
# ggplot(temp0, aes(x=cat_count,y=response.corr,col=condition2)) + stat_summary()



# # # # # # EXPERIMENT 2 # # # # # ####
# # # Total Reversed versus Partial Reversed # # #

# all blocks
temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]

# model adjusted by task_order 
summary(glmer(response.corr~blocksStats*task+task_order+(blocksStats|participant), family = binomial, temp))
# analysis per task order
ggplot(temp, aes(x=blocksStats, y=response.corr, col=condition2)) + facet_grid(.~task_order) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$task_order=="1st task",]))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$task_order=="2nd task",]))
# colSums(table(temp$participant,temp$first)!=0)

# Reviewer 2 remove first trial sensitivity analysis
ggplot(temp[temp$ex_counter!=min(temp$ex_counter),],aes(x=ex_counter,y=response.corr,col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp[temp$ex_counter!=min(temp$ex_counter),]))
# Reviewer 2 analysis of first (pre-feedback) and second (post-feedback) responses 
ggplot(temp[temp$ex_counter<=min(temp$ex_counter)+1,],aes(x=ex_counter,y=response.corr,col=condition2)) + stat_summary(position = position_dodge(.5))
summary(glmer(response.corr~as.factor(ex_counter)*task+(1|participant), family = binomial, temp[temp$ex_counter<=min(temp$ex_counter)+1,]))

# main analysis
m <- glmer(response.corr~blocksStats*task+(blocksStats|participant), family = binomial, temp)
# if singular fit, then try bayesian. Use blme instead of lme4 if glmer is singular=TRUE
# m <- bglmer(response.corr~blocksStats*task+(blocksStats|participant), control = glmerControl(optimizer = "bobyqa"), family = binomial,temp)
summary(m); report::report_table(m)
emmeans(m, pairwise ~ task | blocksStats,type = "response")

# 1. Create a data frame for predictions. We use re.form = NA to get population-level effects (ignoring random intercepts)
temp$pred <- predict(m, newdata = temp, type = "response", re.form = NA)
# 2. Aggregate predictions by blocks (to match the geom_line structure). If your plot separates by condition, aggregate by both.
model_preds <- aggregate(pred ~ blocks + condition2, data = temp, FUN = mean)



# visualization
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
  # geom_line(position = position_dodge(pos)) +
  geom_line(data = model_preds, aes(x = blocks, y = pred, group = condition2),
            position = position_dodge(pos)) +
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
temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]
# first block
m <- glmer(response.corr~task+(1|participant),family = binomial,temp[temp$blocksPhase==1,])
summary(m); report::report_table(m)



# # # # # # Phase 1 and 2: # # # # # #
temp <- lfh[(lfh$blocks==8|lfh$blocks==9) & lfh$condition2!="partial-nonreversed",]
m <- glmer(response.corr~as.factor(blocks)+(1|participant),family = binomial,temp)
summary(m); report::report_table(m)


# # # # # # Phase 2: # # # # # #
# # # Is partial-nonreversed different than 0? # # #
# temp <- lfh[lfh$phase==2 & lfh$blocksPhase <= 2 & lfh$condition2=="partial-nonreversed",]
# m <- glmer(response.corr~blocksPhase+(1|participant),family = binomial,temp)
# summary(m); report::report_table(m)
# # # Is there difference between nonreversed and total reversed in block 11? # # #
# temp <- lfh[lfh$phase==2 & lfh$blocksPhase == 3 & lfh$condition2!="partial-reversed",]
# m <- glmer(response.corr~condition+(1|participant),family = binomial, temp); t.test(response.corr~condition, temp)
# summary(m); report::report_table(m)
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



# # # # # # Phase 2: Reaction Time # # # # # #
# what about reaction time
temp <- lfh[lfh$phase==2 & lfh$condition != "nonreversed",]
m <- lmer(response.rt~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
# no significant interaction, then (Gelman & Hill, 2006)
m <- lmer(response.rt~blocksStats+task+(blocksStats|participant),REML=F,temp)
summary(m); report::report_table(m)
hist(resid(m),20); ks.test(resid(m), "pnorm", mean(resid(m)), sd(resid(m))); shapiro.test(resid(m))
qqnorm(resid(m)); qqline(resid(m), col = "red")

# # # interaction in the first two blocks # # #
# temp <- lfh[lfh$phase==2 & lfh$blocksPhase <= 2 & lfh$condition2!="total-reversed",]
# m <- lmer(response.rt~blocksPhase*condition+(blocksPhase|participant),REML=F,temp)
# summary(m); report::report_table(m)
# temp <- lfh[lfh$phase==2 & lfh$blocksPhase == 2 & lfh$condition2!="total-reversed",]
# m <- lmer(response.rt~condition+(1|participant),REML=F,temp)
# ggplot(temp, aes(x=blocksPhase,y=response.rt,col=condition)) + stat_summary()

m1 <- lmer(log(response.rt)~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m1); report::report_table(m1)
hist(resid(m1),20); ks.test(resid(m1), "pnorm", mean(resid(m1)), sd(resid(m1))); shapiro.test(resid(m1))
qqnorm(resid(m1)); qqline(resid(m1), col = "red")

m2 <- lmer(sqrt(response.rt)~blocksStats*task+(blocksStats|participant),REML=F,temp)
summary(m2); report::report_table(m2)
hist(resid(m2),20); ks.test(resid(m2), "pnorm", mean(resid(m2)), sd(resid(m2))); shapiro.test(resid(m2))
qqnorm(resid(m2)); qqline(resid(m2), col = "red")

m3<- glmer(response.rt~blocksStats*task+(blocksStats|participant),famil=Gamma(link = "log"),temp)
summary(m3); report::report_table(m3)
BIC(m1,m2,m3)



# # # # # # Phase 1: Accuracy # # # # # #
# # # Total Reversed versus Partial Reversed # # #
temp <- lfh[lfh$phase==1 & lfh$condition!="nonreversed",]
# temp <- lfh[lfh$phase==1 & lfh$condition2!="partial-nonreversed",]
m <- glmer(response.corr~blocksPhase*task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)
# no significant interaction, then (Gelman & Hill, 2006)
m <- glmer(response.corr~blocksPhase+task+(blocksPhase|participant),family = binomial,temp)
summary(m); report::report_table(m)
m0 <- glmer(response.corr~blocksStats+(blocksStats|participant),family = binomial,temp)
anova(m,m0); bayesfactor_models(m0, denominator = m)






##### 
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
                        top = text_grob("Experiment 1", color = "black", face = "bold", size = 14))
right <- annotate_figure(ggarrange(figD,figE,figF,nrow=3,
                                   labels = c("D","E","F")),
                         top = text_grob("Experiment 2", color = "black", face = "bold", size = 14))

figure2 <- ggarrange(left,right,ncol=2,common.legend = T)
figure2


# combine
print_fig <- 1
if (print_fig == 1) {
    ggsave("analysis/figure2_v6.pdf", scale=.9,
         plot = figure2, width = 24, height = 24, units = "cm", dpi = 900, 
         limitsize = T)
}





# Sensibility Analysis with ANOVAs# # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# Experiment 1
# collapse
wfe <- lfe %>% group_by(participant, task, phase, condition2, blocks, blocksPhase, blocksStats) %>%
  summarise(pCorr = mean(response.corr,na.rm=T)) %>% ungroup()
ggplot(wfe, aes(x=blocks, y=pCorr, col=condition2)) + stat_summary(geom="line")

# Main ANOVA + plot
tmp <- wfe[wfe$phase == 2 & wfe$condition2 != "partial-nonreversed",]
ggplot(tmp, aes(x=blocksStats, y=pCorr, col=condition2)) + stat_summary(geom="line")
mod <- aov(pCorr ~ condition2 * as.factor(blocksStats) + 
             Error(participant/(condition2 + as.factor(blocksStats))), data = tmp)
summary(mod); effectsize::eta_squared(mod, partial = TRUE, ci.lvl = .95, 
                                      alternative = "two.sided")
# Follow-up ANOVA + plot
tmp <- wfe[wfe$blocksStats == 0 & wfe$phase == 2 & wfe$condition2 != "partial-nonreversed",]
ggplot(tmp, aes(x=condition2, y=pCorr, col=condition2)) + stat_summary()
mod <- aov(pCorr ~ condition2 + Error(participant/(condition2)), data = tmp)
summary(mod); effectsize::eta_squared(mod, partial = TRUE, ci.lvl = .95, 
                                      alternative = "two.sided")

# Experiment 2
# collapse
wfh <- lfh %>% group_by(participant, task, phase, condition2, blocks, blocksPhase, blocksStats) %>%
  summarise(pCorr = mean(response.corr,na.rm=T)) %>% ungroup()
ggplot(wfh, aes(x=blocks, y=pCorr, col=condition2)) + stat_summary(geom="line")

tmp <- wfh[wfh$phase == 2 & wfh$condition2 != "partial-nonreversed",]
ggplot(tmp, aes(x=blocks, y=pCorr, col=condition2)) + stat_summary(geom="line")
mod <- aov(pCorr ~ condition2 * as.factor(blocksStats) + 
             Error(participant/(condition2 + as.factor(blocksStats))), data = tmp)
summary(mod); effectsize::eta_squared(mod, partial = TRUE, ci.lvl = .95, 
                                      alternative = "two.sided")

tmp <- wfe[wfe$blocksStats == 0 & wfe$phase == 2 & wfe$condition2 != "partial-nonreversed",]
ggplot(tmp, aes(x=condition2, y=pCorr, col=condition2)) + stat_summary()
mod <- aov(pCorr ~ condition2 + Error(participant/(condition2)), data = tmp)
summary(mod); effectsize::eta_squared(mod, partial = TRUE, ci.lvl = .95, 
                                      alternative = "two.sided")
