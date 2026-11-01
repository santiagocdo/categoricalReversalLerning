# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)



# read output data
par <- read.csv("../simulations/mod2_partial/exp_partial_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_total/exp_total_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
tot$task <- "total"

source("analysis/functions.R")
db <- combineSimConditions(par,tot) 



# visualize
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
p_act_mod2 <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(#title = expression(Model~2*`;`~rho==0.05*`;`~mu==0.0005),
       title = "Model 2",
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1), xlim=c(25,175)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 125.5, col="black", alpha=0.5) +
  # scale_x_continuous(breaks = seq(0,1000,by=250)) +
  scale_x_continuous(breaks = c(25,125,175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  annotate("text", x = c(80,160), y=c(.05), label=c("Learning","Reversal")) +
  theme_classic() + theme(plot.title = element_text(face = "bold", size = 18))
# p_act_mod2





# read output data
par <- read.csv("../simulations/mod1_partial/exp_partial_mod1_n32_alpha0.2_beta0.9.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod1_total/exp_total_mod1_n32_alpha0.2_beta0.9.csv")
tot$task <- "total"

db <- combineSimConditions(par,tot) 



# visualize
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
p_act_mod1 <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(#title = expression(Model~1*`;`~alpha==0.2*`;`~beta==0.9),
       title = "Model 1",
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1), xlim=c(25,175)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 125.5, col="black", alpha=0.5) +
  # scale_x_continuous(breaks = seq(0,1000,by=250)) +
  scale_x_continuous(breaks = c(25,125,175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  annotate("text", x = c(80,160), y=c(.05), label=c("Learning","Reversal")) +
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", size = 18))   
# p_act_mod1



# # read output data
# par <- read.csv("../simulations/exp_partial5_mod1_n16_alpha0.2_beta0.9.csv")
# par$task <- "partial"
# tot <- read.csv("../simulations/exp_total5_mod1_n16_alpha0.2_beta0.9.csv")
# tot$task <- "total"
# db <- combineSimConditions(par,tot) 
# # visualize
# if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
# db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
# db$condition2 <- paste0(db$task,"-",db$condition)
# db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
# p <- ggplot(db[,], aes(x=nBlock,y=discScore,
#                        col=condition2,linetype=condition2)) +
#   labs(#title = expression(Model~1*`;`~alpha==0.2*`;`~beta==0.9),
#     title = "Model 1",
#     x="blocks",col="Task and Condition",
#     linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
#     y="correct score") +
#   coord_cartesian(ylim = c(0,1), xlim=c(25,175)) +
#   geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
#   geom_vline(xintercept = 125.5, col="black", alpha=0.5) +
#   # scale_x_continuous(breaks = seq(0,1000,by=250)) +
#   scale_x_continuous(breaks = c(25,125,175)) +
#   scale_y_continuous(breaks = seq(0,1,by=0.5)) +
#   scale_color_manual(values = c("blue","orange","orange")) +
#   scale_linetype_manual(values = c("solid","solid","dotted")) +
#   stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
#   stat_summary(geom="errorbar",position = position_dodge(0.2)) +
#   annotate("text", x = c(80,160), y=c(.05), label=c("Learning","Reversal")) +
#   theme_classic() + 
#   theme(plot.title = element_text(face = "bold", size = 16))   
# # p




# # # # # now weights # # # # # 
plotNet <- function (dat, label_manual, categors = NULL) {
  # visualize weights (Input-Hidden)
  rownames(dat) <- dat$X
  dat$X <- NULL
  library(reshape2)
  dat_k1 <- melt(dat[2:9,1:4])
  dat_k1$k <- 1
  dat_k1$inputs <- rownames(dat)[2:9]
  dat_k1$variable <- gsub("[^0-9]", "", dat_k1$variable)
  dat_k1$inputs <- gsub("[^0-9]", "", dat_k1$inputs)
  library(ggplot2)
  p_k1 <- ggplot2::ggplot(dat_k1,aes(x=inputs,y=variable,fill=value)) +
    labs(title = "k=1",x = "input", y = "hidden", fill="weight") +
    geom_tile() + theme_minimal() +
    scale_fill_gradient2(low="red",mid="white",high="green") +
    theme(#axis.text.x = element_text(hjust=1,angle=90),
          legend.position = "none")
  if (!is.null(categors)) {
    p_k1 <- p_k1 +  # --- Add Red Rectangles ---
      # Category 2 (Top Left: inputs 1-4, hidden 3-4)
      geom_rect(aes(xmin = cat$rec2$xmin, xmax = cat$rec2$xmax, 
                    ymin = cat$rec2$ymin, ymax = cat$rec2$ymax), 
                fill = NA, color = "red", size = 1) +
      # Category 1 (Bottom Right: inputs 5-8, hidden 1-2)
      geom_rect(aes(xmin = cat$rec1$xmin, xmax = cat$rec1$xmax, 
                    ymin = cat$rec1$ymin, ymax = cat$rec1$ymax), 
                fill = NA, color = "red", size = 1) +
      # --- Add Text Labels ---
      annotate("text", x = cat$lab2$x, y = cat$lab2$y, 
               label = cat$lab2$label, color = "red", size = 5) +
      annotate("text", x = cat$lab1$x, y = cat$lab1$y, 
               label = cat$lab1$label, color = "red", size = 5)
    # --- Rest of your styling ---
  }
  
  
  dat_k2 <- melt(dat[11:12,1:4])
  dat_k2$k <- 2
  dat_k2$inputs <- rownames(dat)[11:12]
  dat_k2$variable <- gsub("[^0-9]", "", dat_k2$variable)
  dat_k2$inputs <- gsub("[^0-9]", "", dat_k2$inputs)
  p_k2 <- ggplot2::ggplot(dat_k2,aes(x=inputs,y=variable,fill=value)) + 
    labs(title = "k=2",x = "output", y = "hidden", fill="weight") +
    geom_tile() + theme_minimal() +
    scale_fill_gradient2(low="red",mid="white",high="green") +
    theme(#axis.text.x = element_text(hjust=1,angle=90),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
  
  p <- annotate_figure(ggarrange(p_k1, p_k2, widths = c(4,1), ncol=2),
                       top = text_grob(label_manual, color="black", size=12))
  return(p)
}



# model 1 partial condition
ph1 <- read.csv("../simulations/mod1_partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod1_partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
mod1_par <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                            right = text_grob("Partial", color = "black",
                                            face = "bold", size = 14,rot = 270))

# model 1 total condition
ph1 <- read.csv("../simulations/mod1_total/s5_ph1.csv")
cat <- list(rec2 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 6.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 2.5, y = 1.5, label = "Category 1"))
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning", categors = cat)
ph2 <- read.csv("../simulations/mod1_total/s5_ph2.csv")
cat <- list(rec2 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 2.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 6.5, y = 1.5, label = "Category 1"))
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal", categors = cat)
mod1_tot <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                            right = text_grob("Total", color = "black", 
                                            face = "bold", size = 14,rot = 270))

# model 2 partial condition
ph1 <- read.csv("../simulations/mod2_partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod2_partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
mod2_par <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                            right = text_grob("Partial", color = "black",
                                             face = "bold", size = 14,rot = 270))

# model 2 total condition
ph1 <- read.csv("../simulations/mod2_total/s2_ph1.csv")
cat <- list(rec2 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
            rec1 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 0.5, ymax = 1.5),
            lab2 = data.frame(x = 2.5, y = 3, label = "Category 2"),
            lab1 = data.frame(x = 6.5, y = 1, label = "Category 1"))
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning", categors = cat)
ph2 <- read.csv("../simulations/mod2_total/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal", categors = cat)
mod2_tot <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                            right = text_grob("Total", color = "black",
                                            face = "bold", size = 14,rot = 270))



# p <- ggarrange(
#   annotate_figure(ggarrange(mod1_tot,mod1_par,nrow=2),
#                   left = text_grob("Model 1", color = "black",
#                                    face = "bold", size = 18, rot = 90)),
#   annotate_figure(ggarrange(mod2_tot,mod2_par,nrow=2),
#                   left = text_grob("Model 2", color = "black",
#                                    face = "bold", size = 18, rot = 90)),
#   nrow=2)
# p
# print_fig <- 1
# if (print_fig == 1) {
#   ggsave("analysis/figure4.jpg", scale = .8,
#          plot = p, width = 20, height = 24, units = "cm", dpi = 200, 
#          limitsize = T)
# }

legend <- get_legend(p_act_mod1)
p_act_mod1 <- p_act_mod1 + theme(legend.position = "none")
p_act_mod2 <- p_act_mod2 + theme(legend.position = "none")

p <- ggarrange(
  ggarrange(p_act_mod1, legend, p_act_mod2, ncol=3,
            widths = c(2,1,2), labels = c("A","","B")),
  ggarrange(annotate_figure(ggarrange(mod1_tot, mod1_par, nrow=2,
                                      labels = c("C","D")),
                            top = text_grob("Model 1", color = "black",
                                             face = "bold", size = 18)),
            annotate_figure(ggarrange(mod2_tot, mod2_par, nrow=2,
                                      labels = c("E","F")),
                            top = text_grob("Model 2", color = "black",
                                            face = "bold", size = 18)),
            nrow = 2),
  heights = c(1,3), nrow=2
)

print_fig <- 1
if (print_fig == 1) {
  ggsave("analysis/figure4_v2.jpg", scale = 1,
         plot = p, width = 20, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
}


p1 <- ggarrange(p_act_mod1, ggarrange(mod1_tot, mod1_par, nrow=2, labels = c("B","C"),
                                      common.legend = T), 
                labels = c("A",""), nrow=2)
p4 <- ggarrange(p_act_mod2, ggarrange(mod2_tot, mod2_par, nrow=2, labels = c("B","C"),
                                      common.legend = T), 
                labels = c("A",""), nrow=2)
if (print_fig == 1) {
  ggsave("analysis/p1.jpg", scale = 1,
         plot = p1, width = 20, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
  ggsave("analysis/p4.jpg", scale = 1,
         plot = p4, width = 20, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
}



# # # # # Associative # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Learning# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Artificial# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Neural# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Network # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

act_cols <- c("nSubj","ph","nTrial","nBlock","trialType","out","actO","task")
source("analysis/functions.R")

models <- c("Model 1 (backprop)","Model 2 (mu(.05)==rho(.05))","Model 2 (mu(.005)<<rho(.05))",
            "Model 2 (mu(.005)==rho(.005))","Model 2 (mu(.05)>>rho(.005))")

# read output data
par <- read.csv("../simulations/exp_partial_mod1_n16_alpha0.2_beta0.9.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod1_n16_alpha0.2_beta0.9.csv")
tot$task <- "total"
act1 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act1$model <- factor("Model 1 (backprop)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db1 <- rbind(par, tot)
db1$model <- factor("Model 1 (backprop)",levels=models)

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
tot$task <- "total"
act2 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act2$model <- factor("Model 2 (mu(.05)==rho(.05))",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db2 <- rbind(par, tot)
db2$model <- factor("Model 2 (mu(.05)==rho(.05))",levels=models)
# db2 <- db2[,colnames(db1)]

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
tot$task <- "total"
act3 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act3$model <- factor("Model 2 (mu(.005)<<rho(.05))",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db3 <- rbind(par,tot)
db3$model <- factor("Model 2 (mu(.005)<<rho(.05))",levels=models)
# db3 <- db3[,colnames(db1)]

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
tot$task <- "total"
act4 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act4$model <- factor("Model 2 (mu(.005)==rho(.005))",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db4 <- rbind(par,tot)
db4$model <- factor("Model 2 (mu(.005)==rho(.005))",levels=models)

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
tot$task <- "total"
act5 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act5$model <- factor("Model 2 (mu(.05)>>rho(.005))",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db5 <- rbind(par,tot)
db5$model <- factor("Model 2 (mu(.05)>>rho(.005))",levels=models)



library(reshape2)
db <- melt(rbind(db1,db2[,colnames(db1)],db3[,colnames(db1)]), measure.vars = c("deltas.1","deltas.2"))
db$variable <- ifelse(db$variable=="deltas.1", "Input-Hidden", "Hidden-Output")
library(ggplot2)
ggplot(db, aes(x=nBlock, y=value, col=task)) + 
  labs(x="Epochs", y="Average Deltas") +
  geom_vline(xintercept =c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) +
  stat_summary(geom="line") + stat_summary(geom="point") +
  facet_grid(variable ~ model) + 
  theme_classic()

db <- melt(rbind(db1,db2[,colnames(db1)],db3[,colnames(db1)]), measure.vars = c("deltas.3","deltas.4"))
db$variable <- ifelse(db$variable=="deltas.3", "Input-Hidden", "Hidden-Output")
ggplot(db, aes(x=nBlock, y=value, col=task)) + 
  labs(x="Epochs", y="Average dW") +
  geom_vline(xintercept =c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) +
  stat_summary(geom="line") + stat_summary(geom="point") +
  facet_grid(variable ~ model) + 
  theme_classic()

act <- rbind(act1, act2, act3, act4, act5)
act$condition <- factor(act$condition, levels = c("reversed","nonreversed"))
act$condition2 <- paste0(act$task,"-",act$condition)
act$condition2 <- factor(act$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
ggplot(act, aes(x=nBlock, y=discScore, col=condition2)) + 
  labs(x="Epochs", y="Activations") +
  geom_vline(xintercept =c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) +
  stat_summary(geom="line") + stat_summary(geom="point") +
  facet_grid(. ~ model) + 
  theme_classic() + theme(legend.position = "top")







z2 <- melt(db2, measure.vars = c("V1_lrIH","V2_lrIH","V3_lrIH","V4_lrIH",
                                 "V5_lrIH","V6_lrIH","V7_lrIH","V8_lrIH"))
p2_ih <- ggplot(z2, aes(x=nBlock, y=value, col=variable)) + 
  labs(title = "rho (.05) == mu (.05)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) +
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,.41)) +
  theme_bw()
p2_ho <- ggplot(db2, aes(x=nBlock, y=lrHO)) +
  labs(title = "rho (.05) == mu (.05)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw()

z3 <- melt(db3, measure.vars = c("V1_lrIH","V2_lrIH","V3_lrIH","V4_lrIH",
                                 "V5_lrIH","V6_lrIH","V7_lrIH","V8_lrIH"))
p3_ih <- ggplot(z3, aes(x=nBlock, y=value, col=variable)) + 
  labs(title = "rho (.05) >> mu (.005)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,.41)) +
  theme_bw()
p3_ho <- ggplot(db3, aes(x=nBlock, y=lrHO)) +
  labs(title = "rho (.05) >> mu (.005)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw()

z4 <- melt(db4, measure.vars = c("V1_lrIH","V2_lrIH","V3_lrIH","V4_lrIH",
                                 "V5_lrIH","V6_lrIH","V7_lrIH","V8_lrIH"))
p4_ih <- ggplot(z4, aes(x=nBlock, y=value, col=variable)) + 
  labs(title = "rho (.005) == mu (.005)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,.41)) +
  theme_bw()
p4_ho <- ggplot(db4, aes(x=nBlock, y=lrHO)) +
  labs(title = "rho (.005) == mu (.005)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw()

z5 <- melt(db5, measure.vars = c("V1_lrIH","V2_lrIH","V3_lrIH","V4_lrIH",
                                 "V5_lrIH","V6_lrIH","V7_lrIH","V8_lrIH"))
p5_ih <- ggplot(z5, aes(x=nBlock, y=value, col=variable)) + 
  labs(title = "rho (.005) << mu (.05)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,.41)) +
  theme_bw()
p5_ho <- ggplot(db5, aes(x=nBlock, y=lrHO)) +
  labs(title = "rho (.005) << mu (.05)") + stat_summary() +
  geom_vline(xintercept = c(0, 125, 175)) +
  scale_x_continuous(breaks = c(0, 125, 175)) + 
  facet_grid(task ~ .) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw()


ggarrange(p2_ih, p3_ih, p4_ih, p5_ih, ncol=4, common.legend = T)
ggarrange(p2_ho, p3_ho, p4_ho, p5_ho, ncol=4, common.legend = T)



# # # # figure for model type "mod2" (lrIH and lrHO) # # # #
templr <- melt(temp,measure.vars = colnames(temp)[grepl("_lrIH",colnames(temp))])
pLR.IH <- ggplot(templr, aes(x=nBlock,y=value,col=variable)) +
  labs(title="Learning Rates (LR)",
       x = "Blocks (1 block = number of trial types)",
       y="LR.IH",col="Trial Type:") +
  geom_vline(xintercept = cumsum(par$nBlock)) +
  stat_summary(geom="line") + stat_summary() +
  scale_x_continuous(breaks = cumsum(par$nBlock)) + 
  theme_bw()
pLR.HO <- ggplot(temp, aes(x=nBlock,y=lrHO,col=trialType)) +
  labs(title="Learning Rates (LR)",x="Blocks (1 block = n trial types)",
       y="LR.HO",col="Trial Type",shape="Layer LR") +
  geom_vline(xintercept = cumsum(par$nBlock)) +
  stat_summary(geom="line") + stat_summary() +
  scale_x_continuous(breaks = cumsum(par$nBlock)) + 
  theme_bw()
