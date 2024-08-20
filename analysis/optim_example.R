# incredible tutorial at: 
# http://haines-lab.com/post/2018-03-24-human-choice-and-reinforcement-learning-3/

# For pretty plots and data manipulation
library(ggplot2)
library(foreach)
library(dplyr)

# Simulation parameters
set.seed(1)        # Random seed for replication
mu    <- c(1, -1)  # Mean payoff for choices 1 and 2 
sigma <- 3         # SD of payoff distributions
n_tr  <- 200       # Number of trials 
beta  <- 0.1       # True learning rate

# Initial expected value
ev <- c(0, 0) 

# Softmax choice function
logsumexp <- function (x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}
softmax <- function (x) {
  exp(x - logsumexp(x))
}

# Simulate data
sim_dat <- foreach(t=1:n_tr, .combine = "rbind") %do% {
  # Generate choice probability with softmax
  pr <- softmax(ev)
  
  # Use choice probability to sample choice
  choice <- sample(c(1,2), size = 1, prob = pr)
  
  # Generate outcome based on choice
  outcome <- rnorm(1, mean = mu[choice], sd = sigma)
  
  # Delta-rule learning
  ev[choice] <- ev[choice] + beta * (outcome - ev[choice])
  
  # Save data
  data.frame(Trial   = rep(t, 2),
             EV      = ev,
             Pr      = pr,
             Option  = paste(1:2),
             Choice  = rep(choice, 2),
             Outcome = rep(outcome, 2))
}

# Change in expected values across tirals
ggplot(sim_dat, aes(x = Trial, y = EV, geom = "line", color = Option)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  ylab("Expected Value") +
  theme_minimal(base_size = 20)

# Change in probability of selecting each option across tirals
ggplot(sim_dat, aes(x = Trial, y = Pr, geom = "line", color = Option)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  ylab("Pr(Choice)") +
  theme_minimal(base_size = 20)



# # # # # # # # # # Maximum Likelihood Estimation (MLE) # # # # # # # # # # ####
# Define the log-likelihood function used for MLE
mle_bandit <- function(X, beta, outcomes)  {
  # Initialize expected value
  ev <- c(0, 0)
  # loop through each trial and compute log-likelihood
  ll <- foreach(t=seq_along(X), .combine = "c") %do% {
    # Generate choice probability with softmax
    pr <- softmax(ev)
    
    # Delta-rule learning
    ev[X[t]] <- ev[X[t]] + beta * (outcomes[t] - ev[X[t]])
    
    # log probability of "true" simulated choice
    log(pr[X[t]])
  }
  
  # return the summed (minus) log-likelihood, because optim minimizes by default
  sum(-1*ll)
}

# Because data were simulated and output into long format, we need to
# remove every other observation so that we do not double-count
fit_dat <- sim_dat %>%
  filter(Option == 1)

# Use optim to minimize the (minus) log-likelihood function
mle_results <- optim(par      = 0.5,             # Initial guess for beta
                     fn       = mle_bandit,      # Function we are minimizing
                     method   = "L-BFGS-B",      # Specific algorithm used
                     lower    = 0,               # Lower bound for beta 
                     upper    = 1,               # Upper bound for beta
                     X        = fit_dat$Choice,  # Simulated choices
                     outcomes = fit_dat$Outcome) # Simulated choice outcomes

# Print results
cat("The MLE for beta is: " , round(mle_results$par, 3))



# # # # # # # # # # Maximum A Posteriori (MAP) Estimation # # # # # # # # # ####
x <- seq(0, 1, length=1000)
y <- dnorm(x, mean = .15, sd = .25)
qplot(x = x, y = y, geom = "line", xlab = expression(beta[prior]), ylab = "Density") +
  theme_minimal(base_size = 20)

# Define the log-likelihood function used for MAP
map_bandit <- function(X, beta, outcomes)  {
  # Initialize expected value
  ev <- c(0, 0)
  # loop through each trial and compute log-likelihood
  ll <- foreach(t=seq_along(X), .combine = "c") %do% {
    # Generate choice probability with softmax
    pr <- softmax(ev)
    
    # Delta-rule learning
    ev[X[t]] <- ev[X[t]] + beta * (outcomes[t] - ev[X[t]])
    
    # Probability/likelihood of "true" simulated choice
    like <- pr[X[t]]
    
    # Likelihood of current beta according to prior distribution
    prior <- dnorm(x = beta, mean = .15, sd = 0.25)
    
    # Log of like*prior
    log(like*prior)
  }
  
  # return the summed (minus) log-likelihood with prior information included
  sum(-1*ll)
}

# Use optim to minimize the (minus) log-likelihood function
map_results <- optim(par      = 0.5,             # Initial guess for beta
                     fn       = map_bandit,      # Function we are minimizing
                     method   = "L-BFGS-B",      # Specific algorithm used
                     lower    = 0,               # Lower bound for beta 
                     upper    = 1,               # Upper bound for beta
                     X        = fit_dat$Choice,  # Simulated choices
                     outcomes = fit_dat$Outcome) # Simulated choice outcomes

# Print results
cat("The MAP for beta is: " , round(map_results$par, 3))

# Use MLE to fit the first 15 trials
mle_results_15tr <- optim(par      = 0.5,             
                          fn       = mle_bandit,      
                          method   = "L-BFGS-B",      
                          lower    = 0,               
                          upper    = 1,               
                          X        = fit_dat$Choice[1:15],  # Only using first 15 trials
                          outcomes = fit_dat$Outcome[1:15]) 

# Use MAP to fit the first 15 trials
map_results_15tr <- optim(par      = 0.5,             
                          fn       = map_bandit,      
                          method   = "L-BFGS-B",      
                          lower    = 0,               
                          upper    = 1,               
                          X        = fit_dat$Choice[1:15],  # Only using first 15 trials
                          outcomes = fit_dat$Outcome[1:15]) 

cat("The MLE for beta with 15 trials is: " , round(mle_results_15tr$par, 3), "\n", 
    "The MAP for beta with 15 trials is: " , round(map_results_15tr$par, 3))



# # # # # # # # # # Markov Chain Monte Carlo (MCMC) Estimation# # # # # # # ####
# Set number of samples N for the Metropolis algorithm
samples <- 5000

# Set initial guess for beta
beta_n <- 0.5

# Take what we did above for MAP estimation and make into a function
calc_like <- function(beta, X, outcomes) {
  # Initialize expected value
  ev <- c(0, 0)
  # loop through each trial and compute log-likelihood
  ll <- foreach(t=seq_along(X), .combine = "c") %do% {
    # Generate choice probability with softmax
    pr <- softmax(ev)
    
    # Delta-rule learning
    ev[X[t]] <- ev[X[t]] + beta * (outcomes[t] - ev[X[t]])
    
    # Probability/likelihood of "true" simulated choice
    like <- pr[X[t]]
    
    # Likelihood of current beta according to prior distribution
    prior <- dnorm(x = beta, mean = .15, sd = 0.25)
    
    # log of like*prior
    log(like*prior)
  }
  
  # return the summed log-likelihood with prior information included
  sum(ll)
}

# Iterate through N samples and store each result
posterior <- foreach(n=1:samples, .combine = "c") %do% {
  # Step 1: Generate random proposal value with normal distribution
  beta_proposal <- rnorm(1, mean = beta_n, sd = .01)
  
  # If proposal is outside of parameter bounds, keep current sample, else continue
  if (0 < beta_proposal & beta_proposal < 1) {
    # Step 2: Calculate acceptance ratio
    like_proposal <- exp(calc_like(beta_proposal, fit_dat$Choice, fit_dat$Outcome))
    like_current  <- exp(calc_like(beta_n, fit_dat$Choice, fit_dat$Outcome))
    accept <- like_proposal/like_current
    
    # Step 3: Generate uniform random number on [0,1]
    u <- runif(1, min = 0, max = 1)
    
    # Step 4: Accept or reject proposal
    if (u <= accept) {
      beta_n <- beta_proposal
    }
  }
  
  # Return beta_n (either updated with proposal or remains the same)
  beta_n
}

# Plot time-series of posterior samples
qplot(x = 1:samples, y = posterior, geom = "line") + 
  geom_hline(aes(yintercept= beta, linetype = "True Beta"), color= 'red') +
  scale_linetype_manual(name = "", values = 2) +
  xlab("Posterior Sample") +
  ylab(expression(hat(beta))) +
  theme_minimal(base_size = 20)

qplot(posterior[200:5000], geom = "density", fill = I("gray")) +
  geom_line(aes(x = x, y = y), linetype = 2) +
  geom_vline(aes(xintercept= beta, linetype = "True Beta"), color= 'red') +
  scale_linetype_manual(name = "", values = 2) +
  coord_cartesian(xlim = c(0, 1)) +
  xlab(expression(hat(beta))) +
  ylab("Density") +
  theme_minimal(base_size = 20)

