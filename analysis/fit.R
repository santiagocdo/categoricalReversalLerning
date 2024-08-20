# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# visualize using ggplot package
if (!require(mvtnorm)) {install.packages("mvtnorm")}; library(mvtnorm)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(foreach)) {install.packages("foreach")}; library(foreach)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)



################################################################################
############################################################################## #
############################################################################## #

# priors parameters
priors <- list()
priors$mu <- c(.25, 4) # Mean vector (mu)
priors$Sigma <- matrix(c(2, 0, 0, 12), nrow = 2) # Covariance matrix (Sigma)

# priors for alpha
x1 <- seq(0, 1, length=100)
y1 <- dnorm(x1, mean = priors$mu[1], sd = priors$Sigma[1])
qplot(x = x1, y = y1, geom = "line", xlab = expression(alpha[prior]), ylab = "Density") +
  theme_minimal(base_size = 20) + ylim(c(0,.3))
# priors for beta
x2 <- seq(0, 10, length=100)
y2 <- dnorm(x2, mean = priors$mu[2], sd = priors$Sigma[4])
qplot(x = x2, y = y2, geom = "line", xlab = expression(beta[prior]), ylab = "Density") +
  theme_minimal(base_size = 20) + ylim(c(0,.3))

x <- data.frame(x1=rep(x1,each=length(x2)),x2=rep(x2,length(x1)))
x <- data.frame(x,like=mvtnorm::dmvnorm(x, mean = priors$mu, sigma = priors$Sigma))
ggplot(x, aes(x=x1,y=x2,fill=like)) + geom_tile()






mle_map_rw <- function(stim, par, priors = NULL, choices, outcomes)  {
  # transform parameters
  lr <- fromInfTo01(par[1])
  invT <- exp(par[2])
  
  # priors
  if (!is.null(priors)) {
    mu <- priors$mu
    Sigma <- priors$Sigma
    # Likelihood of current beta according to prior distribution
    prior <- mvtnorm::dmvnorm(c(lr, invT), mean = mu, sigma = Sigma)
  } else {
    prior <- 1
  }
  
  # Initial expected value (rows=left and right, cols=fractals)
  W <- matrix(0, nrow=16, ncol=2) 
  # loop through each trial and compute log-likelihood
  ll <- foreach(t = seq_along(stim), .combine = "c") %do% {
    # Generate choice probability with softmax
    pr <- softmax(W[stim[t],], invT)
    
    # prediction error
    pe <- outcomes[t] - W[stim[t],choices[t]]
    
    # Delta-rule learning
    W[stim[t],choices[t]] <- W[stim[t],choices[t]] + lr * pe
    
    # Probability/likelihood of "true" simulated choice
    like <- pr[choices[t]]
    
    # log probability of "true" simulated choice (if no priors, then priors = 1)
    log(like * prior) 
  }
  # return the summed (minus) log-likelihood, because optim minimizes by default
  sum(-1*ll)
}
mle_map_ph <- function(stim, par, priors = NULL, choices, outcomes)  {
  # transform parameters
  lr <- fromInfTo01(par[1])
  invT <- exp(par[2])
  gamma <- fromInfTo01(par[3])
  
  # priors
  if (!is.null(priors)) {
    mu <- priors$mu
    Sigma <- priors$Sigma
    # Likelihood of current beta according to prior distribution
    prior <- mvtnorm::dmvnorm(c(lr, invT), mean = mu, sigma = Sigma)
  } else {
    prior <- 1
  }
  
  # Initial expected value (rows=left and right, cols=fractals)
  W <- A <- matrix(0, nrow=16, ncol=2) 
  # loop through each trial and compute log-likelihood
  ll <- foreach(t=seq_along(stim), .combine = "c") %do% {
    # Generate choice probability with softmax
    pr <- softmax(W[stim[t],], invT)
    # prediction error
    pe <- outcome - W[stim[t],choices[t]]
    # Pearce-Hall Alpha
    A[stim[t],choices[t]] <- gamma * abs(pe) + (1-gamma) * A[stim[t],choices[t]]
    
    # Delta-rule learning
    W[stim[t],choices[t]] <- W[stim[t],choices[t]] + lr * A[stim[t],choices[t]] * pe
    
    # Probability/likelihood of "true" simulated choice
    like <- pr[choices[t]]
    
    # log probability of "true" simulated choice (if no priors, then priors = 1)
    log(like * prior) 
  }
  # return the summed (minus) log-likelihood, because optim minimizes by default
  sum(-1*ll)
}
fromInfTo01 <- function (x) 1 / (1 + exp(-x))
from01toInf <- function (x) log(x / (1-x))
softmax <- function (x, beta) exp(beta*x)/sum(exp(beta*x))

# read behaviour csvs
lf <- read.csv("../categoricalReversalLerning/data/lf_easy.csv")
# lf <- read.csv("../categoricalReversalLerning/data/lf_hard.csv")

# create id
lf$id <- paste0(lf$subjectId,"_",lf$task)

# vector with ids, subjects and task
id <- unique(lf$id)
# id <- id[1:2]

# for loop
for (i in 1:length(id)) {
  message(i)
  # get only one participant
  temp <- lf[lf$id == id[i],]
  # task stimuli order
  stim <- temp$Exemplars
  # choice vector
  Choice <- ifelse(temp$response.keys == "left", 1, 2)
  # outcome vector
  Outcome <- temp$response.corr
  # fit using optim function
  fit_results <- optim(par      = c(0, 0),             # Initial guess for alpha
                       priors   = NULL, # priors,
                       fn       = mle_map_rw,      # Function we are minimizing
                       method   = "BFGS", #"L-BFGS-B",      # Specific algorithm used
                       # control  = list(maxit = 1000, reltol = 1e-8, pgtol = 1e-8, trace = 1, REPORT = 10),
                       # lower    = c(0,0),               # Lower bound for beta
                       # upper    = c(1,20),               # Upper bound for beta
                       stim     = stim,
                       choices  = Choice,  # Simulated choices
                       outcomes = Outcome) # Simulated choice outcomes
  if (i == 1) {
    fits <- data.frame(id=temp$id[1],subjectId=temp$subjectId[1],task=temp$task[1],
                       first=temp$first[1],t(unlist(fit_results)))
  } else {
    fits <- rbind(fits, data.frame(id=temp$id[1],subjectId=temp$subjectId[1],task=temp$task[1],
                                   first=temp$first[1],t(unlist(fit_results))))
  }
}

# check against fits0



################################################################################
alpha_vec <- fits$par1 # rnorm(N,0,1)
beta_vec <- fits$par2 # rnorm(N,0,1)
gamma_vec <- fits$par3 #rnorm(N,0,1)

# each recovery will be drawn 10 times
eachN <- 10

for (i in 1:length(id)) {
  message(i)
  # one subj
  temp <- lf[lf$id == id[i],]
  # task stimuli order
  stim <- temp$Exemplars
  # combine
  out <- as.matrix(cbind(ifelse(temp$r_correct == "left", 1, 0),
                         ifelse(temp$r_correct == "right", 1, 0)))
  
  alpha <- alpha_vec[i] 
  beta <- beta_vec[i]
  gamma <- gamma_vec[i]
  
  for (j in 1:eachN) {
    # Initial expected value (rows=left and right, cols=fractals)
    W <- A <- matrix(0, nrow=16, ncol=2) 
    # Simulate data
    # sim_dat <- foreach(t=seq_along(stim), .combine = "rbind") %do% {
    #   # parameters
    #   lr <- fromInfTo01(alpha)
    #   invT <- exp(beta)
    #   # probability of choosing right (2)
    #   pr <- softmax(W[stim[t],], invT)
    # 
    #   # Use probability to choice (right=1, left=0)
    #   choice <- sample(c(1,2), size = 1, prob = pr)
    # 
    #   # Generate outcome based on choice (if choice == 1 then right, if out == 1 then right),
    #   # so if both are equal then reward outcome == 1
    #   outcome <- as.vector(out[t,choice])
    # 
    #   # Delta-rule learning
    #   W[stim[t],choice] <- W[stim[t],choice] + lr * (outcome - W[stim[t],choice])
    # 
    #   # Save data
    #   data.frame(Trial   = rep(t,2),
    #              Option  = c("left","right"),
    #              Pr      = pr,
    #              Stimulus= rep(stim[t],2),
    #              Choice  = rep(choice,2),
    #              Outcome = rep(outcome,2))
    # }
    sim_dat <- foreach(t=seq_along(stim), .combine = "rbind") %do% {
      # parameters
      lr <- fromInfTo01(alpha)
      invT <- exp(beta)
      gamma <- fromInfTo01(gamma)
      # probability of choosing right (2)
      pr <- softmax(W[stim[t],], invT)

      # Use probability to choice (right=1, left=0)
      choice <- sample(c(1,2), size = 1, prob = pr)

      # Generate outcome based on choice (if choice == 1 then right, if out == 1 then right),
      # so if both are equal then reward outcome == 1
      outcome <- as.vector(out[t,choice])
      # prediction error
      pe <- outcome - W[stim[t],choice]
      # Pearce-Hall Alpha
      A[stim[t],choices[t]] <- gamma * abs(pe) + (1-gamma) * A[stim[t],choices[t]]
      
      # Delta-rule learning
      W[stim[t],choices[t]] <- W[stim[t],choices[t]] + lr * A[stim[t],choices[t]] * pe

      # Save data
      data.frame(Trial   = rep(t,2),
                 Option  = c("left","right"),
                 Pr      = pr,
                 # alpha   = t(as.vector(A)),
                 Stimulus= rep(stim[t],2),
                 Choice  = rep(choice,2),
                 Outcome = rep(outcome,2))
    }
    sim_dat <- cbind(sim_dat,Block=rep(1:12,each=8),Category=temp$r_correct)
    
    ggplot(sim_dat, aes(x=Block,y=Outcome,col=Category)) + stat_summary(geom = "line") +
      stat_summary(position = position_dodge(0.1)) + theme_bw()
    fit_dat <- sim_dat %>%
      filter(Option == "left")
    
    # choice vector
    Choice <- fit_dat$Choice
    # outcome vector
    Outcome <- fit_dat$Outcome
 
    # Use optim to minimize the (minus) log-likelihood function
    rec_results <- optim(par      = c(0, 0),             # Initial guess for alpha
                         priors   = NULL, # priors,
                         fn       = mle_map_rw,      # Function we are minimizing
                         method   = "BFGS", #"L-BFGS-B",      # Specific algorithm used
                         # control  = list(maxit = 1000, reltol = 1e-8, pgtol = 1e-8, trace = 1, REPORT = 10),
                         # lower    = c(0,0),               # Lower bound for beta
                         # upper    = c(1,20),               # Upper bound for beta
                         stim     = stim,
                         choices  = Choice,  # Simulated choices
                         outcomes = Outcome) # Simulated choice outcomes
    if (j == 1) {
      oneSubj <- data.frame(rep=j,t(unlist(rec_results)))
    } else {
      oneSubj <- rbind(oneSubj,data.frame(rep=j,t(unlist(rec_results))))
    }
  }
  # combine recovered subjects parameters
  if (i == 1) {
    recover <- data.frame(id=temp$id[1],subjectId=temp$subjectId[1],task=temp$task[1],
                          first=temp$first[1],oneSubj)
  } else {
    recover <- rbind(recover,data.frame(id=temp$id[1],subjectId=temp$subjectId[1],task=temp$task[1],
                                        first=temp$first[1],oneSubj))
  }
}



for (i in 1:length(id)) {
  temp <- recover[recover$id == id[i],]
  if (i == 1) {
    recover0 <- temp[temp$value == min(temp$value),]
  } else {
    recover0 <- rbind(recover0,temp[temp$value == min(temp$value),])
  }
}


write.csv(fits, "../categoricalReversalLerning/analysis/fits.csv", row.names = F)
write.csv(recover, "../categoricalReversalLerning/analysis/recover.csv", row.names = F)

fits <- read.csv("../categoricalReversalLerning/analysis/fits.csv")
recover <- read.csv("../categoricalReversalLerning/analysis/recover.csv")


plot(fromInfTo01(fits$par1),fromInfTo01(recover0$par1))
cor.test(fromInfTo01(fits$par1),fromInfTo01(recover0$par1))
cbind(fits$par1,recover0$par1)
plot(exp(fits$par2),exp(recover0$par2))
cor.test(exp(fits$par2),exp(recover0$par2))
cbind(fits$par2,recover0$par2)
