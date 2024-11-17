##### Week 10 In-Class Activity #######
##### Date: Nov. 11, 2024 ############


## set simulation parameters
n <- 100 # sample size
pz <- 0.2 # probability of Z = 1
alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
alpha1 <- 1 # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
beta0 <- -3 # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0
beta2 <- 2
## set seed
set.seed(2024)
## generate confounder Z from a binomial distribution
z <- rbinom(n, size = 1, prob = pz)
## compute probability of observing X = 1 from the inverse logit function
px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
## randomly generate binary variable X from the above probability
x <- rbinom(n, size = 1, prob = px)
## randomly generate binary variable Y from the inverse logistic function
py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
y <- rbinom(n, size = 1, prob = py)
## combine three random variables into a data frame
dat <- data.frame(lung = y, coffee = x, smoke = z)
## fit unadjusted logistic regression model
unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
unadj.p <- summary(unadj.mod)$coef[2,4]
unadj.rej <- (unadj.p < 0.05)*1
## fit adjusted logistic regression model
adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
adj.p <- summary(adj.mod)$coef[2,4]
adj.rej <- (adj.p <0.05)*1
c(unadj.rej, adj.rej)

# very simple example
library(foreach)
# set parameters
n <- 100
b0 <- 1; b1 <- 2
# repeat simulation 5 times (combines the bias using the rbind package)
simout <- foreach(i=1:5, .combine=rbind) %do% {         
  set.seed(i + 2024)  # set seed for reproducibility
  x <- rnorm(n)
  y <- b0 + b1*x + rnorm(n)
  bias <- coef(lm(y ~ x))[2] - b1
}
simout


# Use the parallel package

library(parallel)
## set parameters
n <- 100; b0 <- 1; b1 <- 2

## Define the bias function
bias_f <- function(i){
  set.seed(i + 2024)  # set seed for reproducibility
  x <- rnorm(n)
  y <- b0 + b1 * x + rnorm(n)
  bias <- coef(lm(y ~ x))[2] - b1
  return(bias)
}

tictoc::tic() # Start timing

sim_out <- mclapply(
  1:5,         # Input for bias_f
  bias_f,      # Function to repeat
  mc.cores = 5 # Number of cores to use
) %>% unlist() # Simplify result structure

tictoc::toc()  # End timing

######### FINISH SIMULATION STUDY BY CHANGING a1 to 0, 1 and 2 ###################################

library(foreach)
library(knitr)

# Set simulation parameters
n <- 500           # Sample size
pz <- 0.2          # Probability of Z = 1
alpha0 <- 0        # Logit probability of x = 1 in non-smokers (z = 0)
beta0 <- -3        # Logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0         # Coefficient for coffee variable in the logistic model for y
beta2 <- 2         # Coefficient for smoke variable in the logistic model for y


# Run simulation for each alpha1 value

alpha1 <- 0 
simout0 <- foreach(i=1:1000, .combine=rbind) %do% {
  set.seed(2024+i)

  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## randomly generate binary variable Y from the inverse logistic function
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame 
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.p <- summary(unadj.mod)$coef[2,4]
  unadj.rej <- ifelse(unadj.p < 0.05, 1, 0)
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.p <- summary(adj.mod)$coef[2, 4]
  adj.rej <- (adj.p < 0.05) * 1
  c(alpha1, adj.rej, unadj.rej)
}

alpha1 <- 1  # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
simout1 <- foreach(i=1:1000, .combine=rbind) %do% {
  set.seed(2024+i)
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## randomly generate binary variable Y from the inverse logistic function
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame 
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.p <- summary(unadj.mod)$coef[2,4]
  unadj.rej <- ifelse(unadj.p < 0.05, 1, 0)
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.p <- summary(adj.mod)$coef[2, 4]
  adj.rej <- (adj.p < 0.05) * 1
  c(alpha1, adj.rej, unadj.rej)
}

alpha1 <- 2  # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
simout2 <- foreach(i=1:1000, .combine=rbind) %do% {
  set.seed(2024+i)
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## randomly generate binary variable Y from the inverse logistic function
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame 
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.p <- summary(unadj.mod)$coef[2,4]
  unadj.rej <- ifelse(unadj.p < 0.05, 1, 0)
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.p <- summary(adj.mod)$coef[2, 4]
  adj.rej <- (adj.p < 0.05) * 1
  c(alpha1, adj.rej, unadj.rej)
}

simout <- rbind(colMeans(simout0), colMeans(simout1), colMeans(simout2))

kable(simout, col.names = c("$\\alpha_1$", "Adjusted", "Unadjusted "), escape = FALSE, caption = "Type I error rate from 1000 simulations")


