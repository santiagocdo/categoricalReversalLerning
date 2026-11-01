# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# libraries
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)


# selfmade functions
source("analysis/functions.R")

# print figures?
print_fig <- 1

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



# # # # # # # # # # # # Figure 1 # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # Figure 1 # # # # # # # # # # # # # # # # # # # # # # # #
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
fig2A <- ggplot(db[,], aes(x=nBlock,y=discScore,
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
# fig2A

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
fig2B <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(#title = expression(Model~1*`;`~alpha==0.2*`;`~beta==0.9),
    title = "Model 1a",
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
# fig2B



# model 1 partial condition
ph1 <- read.csv("../simulations/mod1_partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod1_partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
fig2C <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
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
fig2D <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Total", color = "black", 
                                           face = "bold", size = 14,rot = 270))
# model 1 partial condition - asymmetrical (input-hidden=.02, hidden-output=0.2)
ph1 <- read.csv("../simulations/mod1_assymetrical_alpha/partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod1_assymetrical_alpha/partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
fig2E <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
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
fig2F <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Total", color = "black",
                                           face = "bold", size = 14,rot = 270))

legend <- get_legend(fig2A)
fig2A <- fig2A + theme(legend.position = "none")
fig2B <- fig2B + theme(legend.position = "none")

fig2 <- ggarrange(
  ggarrange(fig2A, legend, fig2B, ncol=3,
            widths = c(2,1,2), labels = c("A","","B")),
  ggarrange(annotate_figure(ggarrange(fig2C, fig2D, nrow=2,
                                      labels = c("C","D")),
                            top = text_grob("Model 1", color = "black",
                                            face = "bold", size = 18)),
            annotate_figure(ggarrange(fig2E, fig2F, nrow=2,
                                      labels = c("E","F")),
                            top = text_grob("Model 1a (Asymmetric Layers' Fixed Learning Rates)", color = "black",
                                            face = "bold", size = 18)),
            nrow = 2),
  heights = c(1,3), nrow=2
)

if (print_fig == 1) {
  ggsave("analysis/fig2.pdf", scale = 1,
         plot = fig2, width = 20, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
}



# # # # # # # # # # # # Figure 4 # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # Figure 4 # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # Figure 4 # # # # # # # # # # # # # # # # # # # # # # # #
# read output data
par <- read.csv("../simulations/mod2_partial/exp_partial_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_total/exp_total_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
tot$task <- "total"
db <- combineSimConditions(par,tot) 
# visualize
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
fig4A <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(#title = expression(Model~1*`;`~alpha==0.2*`;`~beta==0.9),
    title = "Model 2 (Dynamic Learning Rates)",
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
# fig4A



# model 2 partial condition - asymmetrical (input-hidden=.02, hidden-output=0.2)
ph1 <- read.csv("../simulations/mod2_partial/s3_ph1.csv") #s17
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod2_partial/s3_ph2.csv") #s17
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
fig4B <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Partial", color = "black",
                                           face = "bold", size = 14,rot = 270))
# model 1 total condition - asymmetrical
ph1 <- read.csv("../simulations/mod2_total/s8_ph1.csv")
cat <- list(rec2 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 6.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 2.5, y = 1.5, label = "Category 1"))
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning", categors = cat)
ph2 <- read.csv("../simulations/mod2_total/s8_ph2.csv")
cat <- list(rec2 = data.frame(xmin = 4.5, xmax = 8.5, ymin = 2.5, ymax = 4.5),
            rec1 = data.frame(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 2.5),
            lab2 = data.frame(x = 6.5, y = 3.5, label = "Category 2"),
            lab1 = data.frame(x = 2.5, y = 1.5, label = "Category 1"))
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal", categors = cat)
fig4C <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2, common.legend = T),
                         right = text_grob("Total", color = "black",
                                           face = "bold", size = 14,rot = 270))


fig4 <- ggarrange(
  ggarrange(NULL, fig4A, NULL, ncol=3, labels = c("","A",""), widths = c(1,5,1)),
  ggarrange(fig4B, fig4C, nrow=2, labels = c("B","C")),
  heights = c(2,3), nrow=2)

if (print_fig == 1) {
  # ggsave("analysis/fig4_v2.pdf", scale = 1,
  #        plot = fig4, width = 18, height = 18, units = "cm", dpi = 3000, 
  #        limitsize = T)
   ggsave("analysis/fig4_v2.pdf", scale = .8,
         plot = fig4, width = 21, height = 18, units = "cm", dpi = 3000, 
         limitsize = T)
}



# # # # # # # # # # # # Figure S4 # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # Figure S4 # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # Figure S4 # # # # # # # # # # # # # # # # # # # # # # #

act_cols <- c("nSubj","ph","nTrial","nBlock","trialType","out","actO","task")
source("analysis/functions.R")

models <- c("mu(.05)==rho(.05)","mu(.005)<<rho(.05)",
            "mu(.005)==rho(.005)","mu(.05)>>rho(.005)")


par <- read.csv("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/exp_partial_mod2_n32_beta0.9_rho0.05_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/exp_total_mod2_n32_beta0.9_rho0.05_mu0.05.csv")
tot$task <- "total"
act2 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act2$model <- factor("mu(.05)==rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db2 <- rbind(par, tot)
db2$model <- factor("mu(.05)==rho(.05)",levels=models)

par <- read.csv("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/exp_partial_mod2_n32_beta0.9_rho0.05_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/exp_total_mod2_n32_beta0.9_rho0.05_mu0.005.csv")
tot$task <- "total"
act3 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act3$model <- factor("mu(.005)<<rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db3 <- rbind(par,tot)
db3$model <- factor("mu(.005)<<rho(.05)",levels=models)

par <- read.csv("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/exp_partial_mod2_n32_beta0.9_rho0.005_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/exp_total_mod2_n32_beta0.9_rho0.005_mu0.005.csv")
tot$task <- "total"
act4 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act4$model <- factor("mu(.005)==rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db4 <- rbind(par,tot)
db4$model <- factor("mu(.005)==rho(.005)",levels=models)

par <- read.csv("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/exp_partial_mod2_n32_beta0.9_rho0.005_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/exp_total_mod2_n32_beta0.9_rho0.005_mu0.05.csv")
tot$task <- "total"
act5 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act5$model <- factor("mu(.05)>>rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db5 <- rbind(par,tot)
db5$model <- factor("mu(.05)>>rho(.005)",levels=models)

# figure S2A
act_mod2 <- rbind(act2, act3, act4, act5)
act_mod2$condition <- factor(act_mod2$condition, levels = c("reversed","nonreversed"))
act_mod2$condition2 <- paste0(act_mod2$task,"-",act_mod2$condition)
act_mod2$condition2 <- factor(act_mod2$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
figS2A <- ggplot(act_mod2, aes(x=nBlock, y=discScore, col=condition2, linetype=condition2)) + 
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



par <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/exp_partial_mod2_n32_beta0.9_rho0.05_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/exp_total_mod2_n32_beta0.9_rho0.05_mu0.05.csv")
tot$task <- "total"
act2 <- combineSimConditions(par[,act_cols], tot[,act_cols])
act2$model <- factor("mu(.05)==rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db2 <- rbind(par, tot)
db2$model <- factor("mu(.05)==rho(.05)",levels=models)

par <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/exp_partial_mod2_n32_beta0.9_rho0.05_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/exp_total_mod2_n32_beta0.9_rho0.05_mu0.005.csv")
tot$task <- "total"
act3 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act3$model <- factor("mu(.005)<<rho(.05)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db3 <- rbind(par,tot)
db3$model <- factor("mu(.005)<<rho(.05)",levels=models)

par <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/exp_partial_mod2_n32_beta0.9_rho0.005_mu0.005.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/exp_total_mod2_n32_beta0.9_rho0.005_mu0.005.csv")
tot$task <- "total"
act4 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act4$model <- factor("mu(.005)==rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db4 <- rbind(par,tot)
db4$model <- factor("mu(.005)==rho(.005)",levels=models)

par <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/exp_partial_mod2_n32_beta0.9_rho0.005_mu0.05.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/exp_total_mod2_n32_beta0.9_rho0.005_mu0.05.csv")
tot$task <- "total"
act5 <- combineSimConditions(par[,act_cols],tot[,act_cols])
act5$model <- factor("mu(.05)>>rho(.005)",levels=models)
par <- par[par$out == "actO.1",]; par$out <- par$actO <- NULL
tot <- tot[tot$out == "actO.1",]; tot$out <- tot$actO <- NULL
db5 <- rbind(par,tot)
db5$model <- factor("mu(.05)>>rho(.005)",levels=models)

# figure S2C
act_mod3 <- rbind(act2, act3, act4, act5)
act_mod3$condition <- factor(act_mod3$condition, levels = c("reversed","nonreversed"))
act_mod3$condition2 <- paste0(act_mod3$task,"-",act_mod3$condition)
act_mod3$condition2 <- factor(act_mod3$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
figS2C <- ggplot(act_mod3, aes(x=nBlock, y=discScore, col=condition2, linetype=condition2)) + 
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

(figS2 <- ggarrange(figS2A, figS2C, nrow = 2, common.legend = T))

# if (print_fig == 1) {
#   ggsave("analysis/figS2.pdf", scale = 1,
#          plot = figS2, width = 18, height = 12, units = "cm", dpi = 3000, 
#          limitsize = T)
# }



# model 1 partial condition

# rho.005_mu.005 - Kept Representation
# rho.005_mu.05 - NO
# rho.05_mu.005 - Kept Representation
# rho.05_mu.05 - NO



# -------------------------------------------------------------------------
# 1. Function to Calculate Normalized Frobenius Norm (RMS of weights)
# -------------------------------------------------------------------------
get_normalized_frobenius <- function(W) {
  raw_frobenius <- norm(W, type = "F")
  num_elements <- nrow(W) * ncol(W)
  
  # Divide by the square root of total elements
  normalized_frob <- raw_frobenius / sqrt(num_elements)
  return(normalized_frob)
}
# -------------------------------------------------------------------------
# 2. Function to Calculate Normalized Spectral Norm
# -------------------------------------------------------------------------
get_normalized_spectral <- function(W) {
  raw_spectral <- norm(W, type = "2")
  max_dim <- max(nrow(W), ncol(W))
  
  # Scale by the maximum dimension
  normalized_spec <- raw_spectral / sqrt(max_dim)
  return(normalized_spec)
}



# read model 1, model 1 asym and model 2 and both conditions
fles1p <- list.files("../simulations/mod1_partial/")
fles1p <- paste0("../simulations/mod1_partial/",fles1p); fles1p <- fles1p[-1]
fles1t <- list.files("../simulations/mod1_total/")
fles1t <- paste0("../simulations/mod1_total/",fles1t); fles1t <- fles1t[-1]
fles1ap <- list.files("../simulations/mod1a_partial/")
fles1ap <- paste0("../simulations/mod1a_partial/",fles1ap); fles1ap <- fles1ap[-1]
fles1at <- list.files("../simulations/mod1a_total/")
fles1at <- paste0("../simulations/mod1a_total/",fles1at); fles1at <- fles1at[-1]
fles2p <- list.files("../simulations/mod2_partial/")
fles2p <- paste0("../simulations/mod2_partial/",fles2p); fles2p <- fles2p[-1]
fles2t <- list.files("../simulations/mod2_total/")
fles2t <- paste0("../simulations/mod2_total/",fles2t); fles2t <- fles2t[-1]
# combine them in one vector
fles <- c(fles1p,fles1t,fles1ap,fles1at,fles2p,fles2t)
# keep only csvs
fles <- fles[grepl("csv",fles)]
# Pattern capturing:
# Group 1: Everything inside the simulations subfolder up to the last /
# Group 2: The type folder immediately before the filename (or suffix after mod)
# Group 3: s value
# Group 4: ph value
pattern <- ".*/simulations/(?:(.*)/)?([^/]+)/([^_]+)_([^.]+)\\.csv"

# For paths with nested folders (like mod1_assymetrical_alpha/total), 
# split the dir and file logic cleanly:
parse_simulation_path <- function(path) {
  # 1. Extract filename components (s and ph)
  file_name <- tools::file_path_sans_ext(basename(path))
  file_parts <- unlist(strsplit(file_name, "_"))
  
  # 2. Extract relative directory components after "simulations"
  dir_path <- dirname(path)
  dir_parts <- unlist(strsplit(dir_path, "/"))
  dirs <- dir_parts[!dir_parts %in% c("..", "simulations", ".")]
  
  # 3. Determine mod and type based on directory depth
  if (length(dirs) == 1) {
    # Splits on the LAST underscore (e.g. "mod2_partial" -> "mod2" and "partial")
    mod  <- sub("_[^_]+$", "", dirs[1])
    type <- sub("^.*_", "", dirs[1])
  } else {
    # Handles nested folders (e.g. "mod1_assymetrical_alpha/total")
    mod  <- dirs[1]
    type <- dirs[2]
  }
  
  c(mod = mod, condition = type, s = file_parts[1], ph = file_parts[2])
}

norms <- data.frame(mod=NA, condition=NA, s=NA, ph=NA,
                    norm_frob_1=NA,norm_frob_2=NA,
                    norm_spec_1=NA,norm_spec_2=NA)
norms <-  norms[!is.na(norms),]
for (i in 1:length(fles)) {
  mat <- read.csv(fles[i])
  W1 <- as.matrix(mat[1:9,2:5])
  W2 <- as.matrix(mat[11:12,2:5])
  # norms
  norm_frob_1 <- get_normalized_frobenius(W1)
  norm_frob_2 <- get_normalized_frobenius(W2)
  norm_spec_1 <- get_normalized_spectral(W1)
  norm_spec_2 <- get_normalized_spectral(W2)
  # extract patters from file name
  result <- do.call(rbind, lapply(fles[i], parse_simulation_path))
  # combine and rbind all
  norms <- rbind(norms,data.frame(result,
                                  norm_frob_1,norm_frob_2,
                                  norm_spec_1,norm_spec_2))
}
# prepare data for visualization
library(reshape2)
norms$mod <- ifelse(norms$mod=="mod1","Model 1", ifelse(norms$mod=="mod2", "Model 2", "Model 1a"))
normslf <- melt(norms, measure.vars = c("norm_frob_1","norm_frob_2","norm_spec_1","norm_spec_2"))
normslf$normtype <- substr(normslf$variable,6,9)
normslf$layer <- ifelse(substr(normslf$variable,11,11)=="1","Layer 1","Layer 2")
# figure 0
# norms$norm_frob <- (norms$norm_frob_1+norms$norm_frob_2)/2
# t.test(norm_frob ~ mod, norms[norms$mod != "Model 1a",])
# fig0 <- ggplot(normslf[normslf$ph=="ph1"&normslf$normtype=="frob",], 
#                aes(x=mod,y=value,col=condition,fill=condition,group=condition)) +
#   labs(title="Normalized Frobenius Norm",
#        subtitle = "at the end of learning phase",y="Root Mean Square (RMS)") +
#   stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
#   stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
#                position = position_dodge(width = .95), width = 0.3, color = "black") +
#   scale_color_manual(values = c("orange","blue")) +
#   scale_fill_manual(values = c("orange","blue")) +
#   theme_classic()

# figure A
figA <- ggplot(normslf[normslf$ph=="ph1"&normslf$normtype=="frob",], 
               aes(x=layer,y=value,col=condition,fill=condition,group=condition)) +
  labs(title="Normalized Frobenius Norm",
       subtitle = "at the end of learning phase",y="Root Mean Square (RMS)") +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = .95), width = 0.3, color = "black") +
  scale_color_manual(values = c("orange","blue")) +
  scale_fill_manual(values = c("orange","blue")) +
  facet_grid(.~mod) +
  theme_classic()
# difference score
norms$diff12_norm_frob <- norms$norm_frob_1-norms$norm_frob_2
summary(aov(diff12_norm_frob~mod,norms))
t.test(diff12_norm_frob~condition,norms[norms$mod=="Model 1",])
t.test(diff12_norm_frob~condition,norms[norms$mod=="Model 1a",])
t.test(diff12_norm_frob~condition,norms[norms$mod=="Model 2",])
figB <- ggplot(norms[norms$ph=="ph1",], 
       aes(x=diff12_norm_frob,y=mod,col=condition,fill=condition,group=condition)) +
  labs(title="Difference in Frobenius Norm",
       subtitle = "at the end of learning phase",x="Layer 1 - Layer 2") +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = .95), width = 0.3, color = "black") +
  scale_color_manual(values = c("orange","blue")) +
  scale_fill_manual(values = c("orange","blue")) +
  theme_classic() + theme(axis.title.y = element_blank())
# figB <- figB +
#   # # --- 1. Within Model 1: "ns" Bracket (y = 1) ---
#   # annotate("path", x = c(0.05, 0.1, 0.1, 0.05), y = c(0.78, 0.78, 1.22, 1.22), 
#   #          color = "black", linewidth = 0.5) +
#   # annotate("text", x = 0.15, y = 1.0, label = "***", size = 4, hjust = 0) +
#   # # --- 2. Within Model 2: "***" Bracket (y = 2) ---
#   # annotate("path", x = c(0.05, 0.1, 0.1, 0.05), y = c(1.78, 1.78, 2.22, 2.22),
#   #          color = "black", linewidth = 0.5) +
#   # annotate("text", x = 0.15, y = 2.0, label = "ns", size = 4, hjust = 0) +
#   # --- 3. Between Models: "***" Global Bracket (y = 1 to y = 2) ---
#   annotate("path", x = c(0.28, 0.35, 0.35, 0.28)-.1, y = c(1.0, 1.0, 2.0, 2.0),
#            color = "black", linewidth = 0.5) +
#   annotate("text", x = 0.42-.1, y = 1.5, label = "***", size = 6, angle = 270, vjust = 0.5) + 
#   # --- 4. Extend Canvas Window ---
#   coord_cartesian(xlim = c(-2.2, 0.4-.1)) +
#   scale_x_continuous(breaks = c(-2,-1,0))

fig <- ggarrange(figA, figB, nrow=2, common.legend = T, labels = c("A", "B"), heights = c(2,1))

if (print_fig == 1) {
  ggsave("analysis/figS1.pdf", scale = 1,
         plot = fig, width = 12, height = 16, units = "cm", dpi = 3000, 
         limitsize = T)
}

# figure with spectral norms
(fig <- ggplot(normslf[normslf$ph=="ph1"&normslf$normtype=="spec",], 
               aes(x=layer,y=value,col=condition,fill=condition,group=condition)) +
  labs(title="Normalized Spectral Norm",
       subtitle = "at the end of learning phase",y="Weight Matrix Norm") +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = .95), width = 0.3, color = "black") +
  scale_color_manual(values = c("orange","blue")) +
  scale_fill_manual(values = c("orange","blue")) +
  facet_grid(.~mod) +
  theme_classic())






# # # # # Norms for Model 2 # # # # #
# # # # # Norms for Model 2 # # # # #
# # # # # Norms for Model 2 # # # # #

parse_simulation_path <- function(path) {
  # 1. Extract filename components (s and ph)
  file_name  <- tools::file_path_sans_ext(basename(path))
  file_parts <- unlist(strsplit(file_name, "_"))
  
  # 2. Extract directory components after "simulations"
  dir_path  <- dirname(path)
  dir_parts <- unlist(strsplit(dir_path, "/|\\\\")) # Handles both '/' and '\'
  
  # Find "simulations" folder index
  sim_idx <- which(dir_parts == "simulations")
  
  # Extract all folders after "simulations"
  dirs <- dir_parts[(sim_idx + 1):length(dir_parts)]
  
  # 3. Assign model and condition
  # "model" is the folder right before "condition" (second-to-last directory)
  # "condition" is the folder right before the file (last directory)
  condition <- dirs[length(dirs)]
  model     <- dirs[length(dirs) - 1]
  
  c(model = model, condition = condition, s = file_parts[1], ph = file_parts[2])
}

# read model 1, model 1 asym and model 2 and both conditions
fles1p <- list.files("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/partial/")
fles1p <- paste0("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/partial/",fles1p)
fles1t <- list.files("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/total/")
fles1t <- paste0("../simulations/mod2_pkh_mackintosh/rho.005_mu.005/total/",fles1t)
fles2p <- list.files("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/partial/")
fles2p <- paste0("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/partial/",fles2p)
fles2t <- list.files("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/total/")
fles2t <- paste0("../simulations/mod2_pkh_mackintosh/rho.05_mu.005/total/",fles2t)
fles3p <- list.files("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/partial/")
fles3p <- paste0("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/partial/",fles3p)
fles3t <- list.files("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/total/")
fles3t <- paste0("../simulations/mod2_pkh_mackintosh/rho.05_mu.05/total/",fles3t)
fles4p <- list.files("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/partial/")
fles4p <- paste0("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/partial/",fles4p)
fles4t <- list.files("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/total/")
fles4t <- paste0("../simulations/mod2_pkh_mackintosh/rho.005_mu.05/total/",fles4t)

# combine them in one vector
fles <- c(fles1p,fles1t,fles2p,fles2t,fles3p,fles3t,fles4p,fles4t)
# keep only csvs
fles <- fles[grepl("csv",fles)]
# Pattern capturing:
# Group 1: Everything inside the simulations subfolder up to the last /
# Group 2: The type folder immediately before the filename (or suffix after mod)
# Group 3: s value
# Group 4: ph value
pattern <- ".*/simulations/(?:(.*)/)?([^/]+)/([^_]+)_([^.]+)\\.csv"

norms <- data.frame(mod=NA, condition=NA, s=NA, ph=NA,
                    norm_frob_1=NA,norm_frob_2=NA,
                    norm_spec_1=NA,norm_spec_2=NA); norms <-  norms[!is.na(norms),]
for (i in 1:length(fles)) {
  mat <- read.csv(fles[i])
  W1 <- as.matrix(mat[1:9,2:5])
  W2 <- as.matrix(mat[11:12,2:5])
  # norms
  norm_frob_1 <- get_normalized_frobenius(W1)
  norm_frob_2 <- get_normalized_frobenius(W2)
  norm_spec_1 <- get_normalized_spectral(W1)
  norm_spec_2 <- get_normalized_spectral(W2)
  # extract patters from file name
  result <- do.call(rbind, lapply(fles[i], parse_simulation_path))
  # combine and rbind all
  norms <- rbind(norms,data.frame(result,
                                  norm_frob_1,norm_frob_2,
                                  norm_spec_1,norm_spec_2))
}
# prepare data for visualization
library(reshape2)
normslf <- melt(norms, measure.vars = c("norm_frob_1","norm_frob_2","norm_spec_1","norm_spec_2"))
normslf$model<-factor(normslf$model,levels=c("rho.05_mu.05","rho.05_mu.005",
                                             "rho.005_mu.005","rho.005_mu.05"))
levels(normslf$model) <- models
normslf$normtype <- substr(normslf$variable,6,9)
normslf$layer <- ifelse(substr(normslf$variable,11,11)=="1","Layer 1","Layer 2")

# figure S2B
figS2B <- ggplot(normslf[normslf$ph=="ph1"&normslf$normtype=="frob",], 
               aes(x=layer,y=value,col=condition,fill=condition,group=condition)) +
  labs(title="Normalized Frobenius Norm",
       subtitle = "at the end of learning phase",y="Root Mean Square (RMS)") +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = .95), width = 0.3, color = "black") +
  scale_color_manual(values = c("orange","blue")) +
  scale_fill_manual(values = c("orange","blue")) +
  facet_grid(.~model) +
  theme_classic() + theme(legend.position = "top")



# # # # # Norms for Model 3 # # # # #
# # # # # Norms for Model 3 # # # # #
# # # # # Norms for Model 3 # # # # #

# read model 1, model 1 asym and model 2 and both conditions
fles1p <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/partial/")
fles1p <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/partial/",fles1p)
fles1t <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/total/")
fles1t <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.005/total/",fles1t)
fles2p <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/partial/")
fles2p <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/partial/",fles2p)
fles2t <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/total/")
fles2t <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.005/total/",fles2t)
fles3p <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/partial/")
fles3p <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/partial/",fles3p)
fles3t <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/total/")
fles3t <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.05_mu.05/total/",fles3t)
fles4p <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/partial/")
fles4p <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/partial/",fles4p)
fles4t <- list.files("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/total/")
fles4t <- paste0("../simulations/mod3_mackintosh_antimackintosh/rho.005_mu.05/total/",fles4t)

# combine them in one vector
fles <- c(fles1p,fles1t,fles2p,fles2t,fles3p,fles3t,fles4p,fles4t)
# keep only csvs
fles <- fles[grepl("csv",fles)]
# Pattern capturing:
# Group 1: Everything inside the simulations subfolder up to the last /
# Group 2: The type folder immediately before the filename (or suffix after mod)
# Group 3: s value
# Group 4: ph value
pattern <- ".*/simulations/(?:(.*)/)?([^/]+)/([^_]+)_([^.]+)\\.csv"

norms <- data.frame(mod=NA, condition=NA, s=NA, ph=NA,
                    norm_frob_1=NA,norm_frob_2=NA,
                    norm_spec_1=NA,norm_spec_2=NA); norms <-  norms[!is.na(norms),]
for (i in 1:length(fles)) {
  mat <- read.csv(fles[i])
  W1 <- as.matrix(mat[1:9,2:5])
  W2 <- as.matrix(mat[11:12,2:5])
  # norms
  norm_frob_1 <- get_normalized_frobenius(W1)
  norm_frob_2 <- get_normalized_frobenius(W2)
  norm_spec_1 <- get_normalized_spectral(W1)
  norm_spec_2 <- get_normalized_spectral(W2)
  # extract patters from file name
  result <- do.call(rbind, lapply(fles[i], parse_simulation_path))
  # combine and rbind all
  norms <- rbind(norms,data.frame(result,
                                  norm_frob_1,norm_frob_2,
                                  norm_spec_1,norm_spec_2))
}
# prepare data for visualization
library(reshape2)
normslf <- melt(norms, measure.vars = c("norm_frob_1","norm_frob_2","norm_spec_1","norm_spec_2"))
normslf$model<-factor(normslf$model,levels=c("rho.05_mu.05","rho.05_mu.005",
                                             "rho.005_mu.005","rho.005_mu.05"))
levels(normslf$model) <- models
normslf$normtype <- substr(normslf$variable,6,9)
normslf$layer <- ifelse(substr(normslf$variable,11,11)=="1","Layer 1","Layer 2")

# figure S2D
figS2D <- ggplot(normslf[normslf$ph=="ph1"&normslf$normtype=="frob",], 
               aes(x=layer,y=value,col=condition,fill=condition,group=condition)) +
  labs(title="Normalized Frobenius Norm",
       subtitle = "at the end of learning phase",y="Root Mean Square (RMS)") +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(.95),alpha=.2) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = .95), width = 0.3, color = "black") +
  scale_color_manual(values = c("orange","blue")) +
  scale_fill_manual(values = c("orange","blue")) +
  facet_grid(.~model) +
  theme_classic() + theme(legend.position = "top")

figS2 <- ggarrange(figS2A, figS2D, figS2C, figS2D, nrow = 4, common.legend = T,
                   labels = LETTERS[1:4])

if (print_fig == 1) {
  ggsave("analysis/figS2_v2.pdf", scale = 1,
         plot = figS2, width = 18, height = 24, units = "cm", dpi = 3000, 
         limitsize = T)
}


# N <- 3000
# # 1. Pre-allocate list elements to their final size
# W <- list(
#   numeric(N), # Vector for Layer 1 norms
#   numeric(N), # Vector for Layer 2 norms
#   numeric(N)  # Vector for Layer 3 norms
# )
# # 2. Run the loop with direct index assignment
# for (i in 1:N) {
#   # Generate matrices and calculate norms in a single step
#   W[[1]][i] <- norm(matrix(rnorm(4,1,1), 2, 2), type = "F")
#   W[[2]][i] <- norm(matrix(rnorm(16,1,1), 4, 4), type = "F")
#   W[[3]][i] <- norm(matrix(rnorm(256,1,1), 16, 16), type = "F")
# }
# 
# mean(W[[1]],na.rm = T)
# mean(W[[2]],na.rm = T)
# mean(W[[3]],na.rm = T)
# 
# mean(W[[1]]/sqrt(4),na.rm = T)
# mean(W[[2]]/sqrt(16),na.rm = T)
# mean(W[[3]]/sqrt(256),na.rm = T)

