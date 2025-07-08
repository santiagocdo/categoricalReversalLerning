# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))



# read output data
par <- read.csv("../simulations/mod2_partial/exp_partial_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
par$task <- "partial"
tot <- read.csv("../simulations/mod2_total/exp_total_mod2_n32_beta0.9_rho0.05_mu5e-04.csv")
tot$task <- "total"

source("analysis/functions.R")
db <- combineSimConditions(par,tot) 



# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
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
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
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
  theme(plot.title = element_text(face = "bold", size = 16))   
# p_act_mod1



# # # # # now weights # # # # # 
plotNet <- function (dat, label_manual) {
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
mod1_par <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2),
                            right = text_grob("Partial", color = "black",
                                            face = "bold", size = 14,rot = 270))

# model 1 total condition
ph1 <- read.csv("../simulations/mod1_total/s5_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod1_total/s5_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
mod1_tot <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2),
                            right = text_grob("Total", color = "black", 
                                            face = "bold", size = 14,rot = 270))

# model 2 partial condition
ph1 <- read.csv("../simulations/mod2_partial/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod2_partial/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
mod2_par <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2),
                            right = text_grob("Partial", color = "black",
                                             face = "bold", size = 14,rot = 270))

# model 2 total condition
ph1 <- read.csv("../simulations/mod2_total/s2_ph1.csv")
p_ph1 <- plotNet(dat = ph1, label_manual = "Learning")
ph2 <- read.csv("../simulations/mod2_total/s2_ph2.csv")
p_ph2 <- plotNet(dat = ph2, label_manual = "Reversal")
mod2_tot <- annotate_figure(ggarrange(p_ph1, p_ph2, ncol=2),
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

library(ggpubr)
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




