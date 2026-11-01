# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# libraries
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)


# selfmade functions
source("analysis/functions.R")


# # # # # # # # # # # # Figure 1 # # # # # # # # # # # # # # # # # # # # # # # #
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
fig1A <- ggplot(db[,], aes(x=nBlock,y=discScore,
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
# fig1A

# read output data
par <- read.csv("../simulations/mod1_assymetrical_alpha/exp_partial_mod1_n16_alpha0.2_beta0.9.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod1_assymetrical_alpha/exp_total_mod1_n16_alpha0.2_beta0.9.csv")
tot$task <- "total"
db <- combineSimConditions(par,tot) 
# visualize
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
fig1B <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(#title = expression(Model~1*`;`~alpha==0.2*`;`~beta==0.9),
    title = "Model 1 (Asymetrical LR)",
    x="blocks",col="Task and Condition",
    linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
    y="correct score") +
  coord_cartesian(ylim = c(0,1), xlim=c(50,400)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 250.5, col="black", alpha=0.5) +
  # scale_x_continuous(breaks = seq(0,1000,by=250)) +
  scale_x_continuous(breaks = c(50,250,400)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  annotate("text", x = c(150,325), y=c(.05), label=c("Learning","Reversal")) +
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", size = 18))   
# fig1B



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
fig1C <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
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
fig1D <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Total", color = "black", 
                                           face = "bold", size = 14,rot = 270))
# model 1 partial condition - asymmetrical
ph1 <- read.csv("../simulations/mod1_assymetrical_alpha/partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod1_assymetrical_alpha/partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
fig1E <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Partial", color = "black",
                                           face = "bold", size = 14,rot = 270))
# model 1 total condition - asymmetrical
ph1 <- read.csv("../simulations/mod1_assymetrical_alpha/total/s4_ph1.csv")
cat <- list(rec2 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 6.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 2.5, y = 1.5, label = "Category 1"))
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning", categors = cat)
ph2 <- read.csv("../simulations/mod1_assymetrical_alpha/total/s4_ph2.csv")
cat <- list(rec2 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 2.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 6.5, y = 1.5, label = "Category 1"))
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal", categors = cat)
fig1F <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
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

legend <- get_legend(fig1A)
fig1A <- fig1A + theme(legend.position = "none")
fig1B <- fig1B + theme(legend.position = "none")

fig1 <- ggarrange(
  ggarrange(fig1A, legend, fig1B, ncol=3,
            widths = c(2,1,2), labels = c("A","","B")),
  ggarrange(annotate_figure(ggarrange(fig1C, fig1D, nrow=2,
                                      labels = c("C","D")),
                            top = text_grob("Model 1", color = "black",
                                            face = "bold", size = 18)),
            annotate_figure(ggarrange(fig1E, fig1F, nrow=2,
                                      labels = c("E","F")),
                            top = text_grob("Model 1 (Asymetrical LR)", color = "black",
                                            face = "bold", size = 18)),
            nrow = 2),
  heights = c(1,3), nrow=2
)

print_fig <- 1
if (print_fig == 1) {
  ggsave("analysis/fig1.pdf", scale = 1,
         plot = fig1, width = 20, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
}



# # # # # Associative # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Learning# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Artificial# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Neural# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Network # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

act_cols <- c("nSubj","ph","nTrial","nBlock","trialType","out","actO","task")
source("analysis/functions.R")



models <- c("mu(.05)==rho(.05)","mu(.005)<<rho(.05)",
            "mu(.005)==rho(.005)","mu(.05)>>rho(.005)")

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
tot$task <- "total"
act2 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act2$model <- factor("mu(.05)==rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db2 <- rbind(par, tot)
db2$model <- factor("mu(.05)==rho(.05)",levels=models)

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
tot$task <- "total"
act3 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act3$model <- factor("mu(.005)<<rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db3 <- rbind(par,tot)
db3$model <- factor("mu(.005)<<rho(.05)",levels=models)

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
tot$task <- "total"
act4 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act4$model <- factor("mu(.005)==rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db4 <- rbind(par,tot)
db4$model <- factor("mu(.005)==rho(.005)",levels=models)

par <- read.csv("../simulations/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/exp_total_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
tot$task <- "total"
act5 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act5$model <- factor("mu(.05)>>rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db5 <- rbind(par,tot)
db5$model <- factor("mu(.05)>>rho(.005)",levels=models)



act_mod2 <- rbind(act2, act3, act4, act5)
act_mod2$condition <- factor(act_mod2$condition, levels = c("reversed","nonreversed"))
act_mod2$condition2 <- paste0(act_mod2$task,"-",act_mod2$condition)
act_mod2$condition2 <- factor(act_mod2$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
fig4A <- ggplot(act_mod2, aes(x=nBlock, y=discScore, col=condition2, linetype=condition2)) + 
  labs(title = "Model 2", x="blocks", y="correct score",
       linetype="Task and Condition",col="Task and Condition") +
  geom_vline(xintercept =c(125, 175)) +
  coord_cartesian(ylim = c(0,1), xlim=c(25,175)) +
  scale_x_continuous(breaks = c(25, 125, 175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1) +
  # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_grid(. ~ model) + 
  theme_classic() + theme(legend.position = "top",
                          plot.title = element_text(face = "bold", size = 18))



models <- c("mu(.05)==rho(.05)","mu(.005)<<rho(.05)",
            "mu(.005)==rho(.005)","mu(.05)>>rho(.005)")

par <- read.csv("../simulations/mackintosh_antimackintosh/rho.05_mu.05/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mackintosh_antimackintosh/rho.05_mu.05/exp_total_mod2_n16_beta0.9_rho0.05_mu0.05.csv")
tot$task <- "total"
act2 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act2$model <- factor("mu(.05)==rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db2 <- rbind(par, tot)
db2$model <- factor("mu(.05)==rho(.05)",levels=models)

par <- read.csv("../simulations/mackintosh_antimackintosh/rho.05_mu.005/exp_partial_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mackintosh_antimackintosh/rho.05_mu.005/exp_total_mod2_n16_beta0.9_rho0.05_mu0.005.csv")
tot$task <- "total"
act3 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act3$model <- factor("mu(.005)<<rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db3 <- rbind(par,tot)
db3$model <- factor("mu(.005)<<rho(.05)",levels=models)

par <- read.csv("../simulations/mackintosh_antimackintosh/rho.005_mu.005/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mackintosh_antimackintosh/rho.005_mu.005/exp_total_mod2_n16_beta0.9_rho0.005_mu0.005.csv")
tot$task <- "total"
act4 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act4$model <- factor("mu(.005)==rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db4 <- rbind(par,tot)
db4$model <- factor("mu(.005)==rho(.005)",levels=models)

par <- read.csv("../simulations/mackintosh_antimackintosh/rho.005_mu.05/exp_partial_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mackintosh_antimackintosh/rho.005_mu.05/exp_total_mod2_n16_beta0.9_rho0.005_mu0.05.csv")
tot$task <- "total"
act5 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act5$model <- factor("mu(.05)>>rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db5 <- rbind(par,tot)
db5$model <- factor("mu(.05)>>rho(.005)",levels=models)



act_mod3 <- rbind(act2, act3, act4, act5)
act_mod3$condition <- factor(act_mod3$condition, levels = c("reversed","nonreversed"))
act_mod3$condition2 <- paste0(act_mod3$task,"-",act_mod3$condition)
act_mod3$condition2 <- factor(act_mod3$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
fig4B <- ggplot(act_mod3, aes(x=nBlock, y=discScore, col=condition2, linetype=condition2)) + 
  labs(title = "Model 3", x="blocks", y="correct score",
       linetype="Task and Condition",col="Task and Condition") +
  geom_vline(xintercept =c(125, 175)) +
  coord_cartesian(ylim = c(0,1), xlim=c(25,175)) +
  scale_x_continuous(breaks = c(25, 125, 175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1) +
  # stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  facet_grid(. ~ model) + 
  theme_classic() + theme(legend.position = "top",
                          plot.title = element_text(face = "bold", size = 18))





fig <- ggarrange(fig4A, fig4B, nrow = 2, common.legend = T)

print_fig <- 1
if (print_fig == 1) {
  ggsave("analysis/figSuper.pdf", scale = 1,
         plot = fig, width = 18, height = 12, units = "cm", dpi = 3000, 
         limitsize = T)
}



# model 1 partial condition

# rho.005_mu.005 - Kept Representation
# rho.005_mu.05 - NO
# rho.05_mu.005 - Kept Representation
# rho.05_mu.05 - NO

