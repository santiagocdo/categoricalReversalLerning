# interactions?

N <- 11
# CS+, NoS=58; 
# CS+, OpS=39; 
# CS-, NoS=38, 
# CS-, OpS=37;
# error mean square
ems <- 15#sqrt(275)

# set.seed(1)
data <- rbind(data.frame(id=1:N, stimuli="CS+", optogen="NoS", resp=rnorm(N, 58, ems)),
              data.frame(id=1:N, stimuli="CS+", optogen="OpS", resp=rnorm(N, 39, ems)),
              data.frame(id=1:N, stimuli="CS-", optogen="NoS", resp=rnorm(N, 38, ems)),
              data.frame(id=1:N, stimuli="CS-", optogen="OpS", resp=rnorm(N, 37, ems)))
# Classic ANOVA
m <- aov(resp ~ stimuli * optogen + Error(id/(stimuli*optogen)), data)
summary(m)


library(ggplot2)

ggplot(data, aes(x=stimuli,y=resp,col=optogen,fill=optogen)) + 
  stat_summary(geom="bar", position = position_dodge(.8)) +
  stat_summary(geom="errorbar", col="black", width=.2, position = position_dodge(.8))

library(lmerTest)
model0 <- lmer(resp ~ 1 + (1|id), data)
model1 <- lmer(resp ~ stimuli + optogen + (1|id), data)
model2 <- lmer(resp ~ stimuli * optogen + (1|id), data)
summary(model1)
anova(model1)
anova(model0,model1,model2)

# Classic ANOVA
m <- aov(resp ~ stimuli * optogen + Error(id/(stimuli*optogen)), data)
summary(m)

data$resp_hat <- predict(model1)
data$x_plot <- ifelse(data$test=="CS+", 1, 0)

ggplot(data, aes(x=x_plot, y=resp, col=stim, fill=stim)) + 
  stat_summary() +# Model predictions
  # geom_line(data, aes(x=x_plot, group = id, x, y=resp_hat)) #+
  scale_x_continuous(limits = c(-.2,1.2),
                     breaks = c(0,1), labels=c("CS+","CS-"))+
  geom_smooth(method = "lm")
