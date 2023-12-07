# Load necessary libraries
library(Matrix)
library(lme4)
library(statmod)
library(fields)
library(mvtnorm)

# 1. Create a spatial GLMM toy dataset
set.seed(123)
n_locations <- 100

## Create two predictors X1 and X2
X1 <- runif(n_locations, -1, 1)
X2 <- rnorm(n_locations, 0, 1)

## True values of the parameters
beta0 <- log(5)
beta1 <- 0.5
beta2 <- -0.3
sigma2_spatial <- 1
phi <- 1
coordinates <- cbind(runif(n_locations, 0, 50), runif(n_locations, 0, 50))

# Create a function to simulate spatial random effects (multivariate normal distribution)
spatial_u <- function(sigma2_spatial, phi, coordinates){
  # Simulate spatial random effect using a Gaussian field
  cov_matrix <- sigma2_spatial * exp(-1 * (as.matrix(dist(coordinates)))/phi) # Exponential decay of correlation with distance
  chol_matrix <- chol(cov_matrix) # Cholesky decomposition
  spatial_effect <- chol_matrix %*% rnorm(n_locations)
  return(spatial_effect)
}
u <- spatial_u(sigma2_spatial=1, phi=1, coordinates)

# Simulate response variable (Poisson-distributed count data)
lambda <- exp(beta0 + beta1 * X1 + beta2 * X2 + u)  # Poisson rate
y <- rpois(n_locations, lambda)
data <- data.frame(y, X1, X2, coordinates)
colnames(data) <- c("y", "X1", "X2", "coord_1", "coord_2")

# EM Algorithm
# Design Matrices for Fixed Effects
X <- model.matrix(~ X1 + X2, data = data)  # Fixed effects

# Initialize Parameters
beta <- rep(0, ncol(X))                  # Fixed effects
sigma2_spatial <- 1                      # Variance for spatial random effects
n_locations <- nrow(data)
spatial_effect <- rep(0, n_locations)    # Spatial random effects

# Convergence Settings
max.iter <- 3000
tolerance <- 1e-8
conv <- FALSE

exp(beta[1] + beta[2]*data$X1[3] + beta[3]*data$X2[3] + u_est[3])
exp(beta[1] + beta[2]*data$X1 + beta[3]*data$X2 + u_est)
exp(-50)
head(data)
cov_matrix <- sigma2_spatial * exp(-phi * distances)
##############################
# Convergence Settings
max.iter <- 10000
tolerance <- 1e-5
conv <- FALSE
distances <- as.matrix(dist(cbind(data$coord_1, data$coord_2)))

# Design Matrices for Fixed Effects
X <- model.matrix(~ X1 + X2, data = data)  # Fixed effects

# Initialize Parameters
beta <- rep(0, ncol(X))                  # Fixed effects
sigma2_spatial <- 1                      # Variance for spatial random effects
phi <- 1
n_locations <- nrow(data)
u_est <- rep(0, n_locations)    # Spatial random effects

# EM Algorithm
for (iter in 1:max.iter) {
  sigma2_spatial_old <- sigma2_spatial
  phi_old <- phi
  beta_old <- beta
  u_est_old <- u_est
  
  # E-Step: Estimate spatial random effects)
  cov_matrix <- sigma2_spatial_old * exp(-1*distances/phi_old) # Exponential decay of correlation with distance
  mu <- X %*% beta_old
  u_est <- solve(cov_matrix, (log(data$y + 1) - mu))
  
  # Debugging code
  if (any(is.nan(u_est)) || any(is.infinite(u_est))) {
    cat("Issue at iteration", iter, "\n")
    break
  }
  
  # M-Step: Update fixed effects
  beta <- solve(crossprod(X), t(X) %*% (log(data$y + 1) - u_est))
  
  # Check for convergence
  if (max(abs(u_est - u_est_old), na.rm = TRUE) < tolerance) {
    conv <- TRUE
    cat("Converged in", iter, "iterations\n")
    break
  }
}
beta
print(beta)
# Output
if (conv) {
  cat("Converged in", iter, "iterations\n")
} else {
  cat("Did not converge\n")
}

cat("Fixed Effects (Beta):\n")
print(beta)

cat("Random Effects (u):\n")
print(u)


beta=c(0.1, 0.2, 0.3)
cov_matrix <- sigma2_spatial * exp(-1*distances/phi) # Exponential decay of correlation with distance
mu <- X %*% beta
u_est <- solve(cov_matrix, (log(data$y + 0.01) - mu))


##############################
  
  # M-Step: Update fixed effects
  beta_new <- solve(X, (log(data$response + 1) - spatial_effect))

  # Check for convergence
  if (max(abs(spatial_effect - spatial_effect_old)) < tolerance) {
    conv <- TRUE
    break
  }
}
##############################


# EM Algorithm
for (iter in 1:max.iter) {
  # E-Step: Estimate spatial random effects
  spatial_effect_old <- spatial_effect

  # This is a simplification and should be adapted to your specific spatial model
  for (i in 1:n_locations) {
    distances <- as.matrix(dist(cbind(data$coord_1, data$coord_2)))
    cov_matrix <- sigma2_spatial * exp(-phi * distances) # Exponential decay of correlation with distance
    spatial_effect[i] <- rmvnorm(1, mean = rep(0, n_locations), sigma = cov_matrix) %*% rnorm(n_locations)
  }
  
  # M-Step: Update fixed effects
  beta <- solve(crossprod(X), t(X) %*% (log(data$response + 1) - spatial_effect))
  X %*% t(X)
  # Check for convergence
  if (max(abs(spatial_effect - spatial_effect_old)) < tolerance) {
    conv <- TRUE
    break
  }
}

# Output
if (conv) {
  cat("Converged in", iter, "iterations\n")
} else {
  cat("Did not converge\n")
}

cat("Fixed Effects (Beta):\n")
print(beta)

cat("Spatial Random Effects:\n")
print(spatial_effect)


# MCEM Algorithm
set.seed(123)
n_locations <- 100

# Simulate fixed effect data (e.g. a continuous predictor)
fixed_effect <- runif(n_locations, 1, 10)

# Simulate spatial coordinates
coordinates <- cbind(runif(n_locations, 0, 100), runif(n_locations, 0, 100))

# Simulate response variable
true_spatial_effect <- rnorm(n_locations)
response <- rpois(n_locations, lambda = exp(0.3*fixed_effect + true_spatial_effect))

data <- data.frame(response, fixed_effect, coordinates)

# Initialize parameters
beta <- 0
sigma2_spatial <- 1

# EM algorithm settings
max_iter <- 10
n_monte_carlo_samples <- 1000

for (iter in 1:max_iter) {
  # E-Step: Monte Carlo simulation to estimate spatial random effects
  spatial_effects_samples <- matrix(NA, n_locations, n_monte_carlo_samples)
  
  for (i in 1:n_locations) {
    # Simulate spatial random effects using Monte Carlo
    # Assuming a normal distribution for simplicity
    spatial_effects_samples[i,] <- rnorm(n_monte_carlo_samples, mean=0, sd = sqrt(sigma2_spatial))
  }
  
  # Estimate spatial effect as the mean of the samples
  estimated_spatial_effect <- rowMeans(spatial_effects_samples)
  
  # Recalculate the response adjustment with he updated beta
  response_adjusted <- log(data$response + 1) - data$fixed_effect * beta
  
  # M-Step: Update fixed effect (beta)
  # Using a simple linear regression model for illustration
  fitted_model <- lm(response_adjusted ~ estimated_spatial_effect - 1)
  beta_new <- coef(fitted_model)
  
  # Update the variance of spatial random effects (sigma2_spatial)
  residuals <- residuals(fitted_model)
  sigma2_spatial_new <- var(residuals)
  
  # Convergence check (for both beta and sigma2_spatial)
  if (abs(beta_new - beta) < tolerance && abs(sigma2_spatial_new - sigma2_spatial) < tolerance) {
    beta <- beta_new
    sigma2_spatial <- sigma2_spatial_new
    break
  }
  
  # Update parameters for the next iteration
  beta <- beta_new
  sigma_spatial <- sigma2_spatial_new
}

print(paste("Beta:", beta))
print(paste("Sigma2 (Spatial):", sigma2_spatial))


############################

# 1. Simulate data
set.seed(123)
n_locations <- 100  # Number of spatial locations

# Simulate a fixed effect (e.g., a continuous environmental variable)
fixed_effect <- runif(n_locations, 1, 10)

# Simulate spatial coordinates (e.g., in a 2D space)
coordinates <- cbind(runif(n_locations, 0, 100), runif(n_locations, 0, 100))

# True spatial random effects (unknown in real scenarios)
true_spatial_effect <- rnorm(n_locations, mean = 0, sd = 2)

# Simulate response variable (Poisson-distributed count data)
lambda <- exp(0.4 * fixed_effect + true_spatial_effect)  # Poisson rate
response <- rpois(n_locations, lambda)

data <- data.frame(response, fixed_effect, coordinates)

# Initialize parameters
beta <- 0.1
spatial_effects <- rep(0, n_locations)
last_mcmc_samples <- rep(0, n_locations)

# EM Algorithm settings
max_iter <- 20
burn_in <- 1000
n_mcmc <- 2000

# Proposal and acceptance functions
# Function to calculate the proposal distribution
proposal_function <- function(current_effect) {
  return(rnorm(1, mean = current_effect, sd = 1))
}

# Function to calculate acceptance probability
acceptance_probability <- function(proposed_effect, current_effect, beta, response, fixed_effect) {
  # Calculate the likelihood ratio
  proposed_likelihood <- sum(dpois(response, lambda = exp(beta * fixed_effect + proposed_effect), log = TRUE))
  current_likelihood <- sum(dpois(response, lambda = exp(beta * fixed_effect + current_effect), log = TRUE))
  return(min(1, exp(proposed_likelihood - current_likelihood)))
}

# Start EM Algorithm
for (iter in 1:max_iter) {
  
  # E-Step: Metropolis-Hastings for spatial random effects
  for (loc in 1:n_locations) {
    current_effect <- last_mcmc_samples[loc]
    spatial_effect_sum <- 0 # Reset sum for averaging
    
    for (mcmc_iter in 1:n_mcmc) {
      proposed_effect <- proposal_function(current_effect)
      alpha <- acceptance_probability(proposed_effect, current_effect, beta,
                                      data$response, data$fixed_effect)
      if (runif(1) < alpha) {
        current_effect <- proposed_effect
      }
      if (mcmc_iter > burn_in) {
        spatial_effect_sum <- spatial_effect_sum + current_effect
      }
    }
    
    # Update spatial effect estimate and store the last MCMC sample
    spatial_effects[loc] <- spatial_effect_sum / (n_mcmc - burn_in)
    last_mcmc_samples[loc] <- current_effect
  }
  
  # M-Step: Update fixed effect (beta)
  fitted_model <- lm(log(response+1) ~ fixed_effect + spatial_effects - 1, data = data)
  beta <- coef(fitted_model)['fixed_effect']
}

# Final Parameter Estimates
print(paste("Beta:", beta))
print("Spatial Effects:")
print(spatial_effects)
mean(spatial_effects)



##############################
# Conventional EM Algorithm
# Convergence Settings
max.iter <- 10000
tolerance <- 1e-5
conv <- FALSE
distances <- as.matrix(dist(cbind(data$coord_1, data$coord_2)))

# Design Matrices for Fixed Effects
X <- model.matrix(~ X1 + X2, data = data)  # Fixed effects

# Initialize Parameters
beta <- rep(0, ncol(X))                  # Fixed effects
sigma2_spatial <- 1                      # Variance for spatial random effects
phi <- 1
n_locations <- nrow(data)
u_est <- rep(0, n_locations)    # Spatial random effects
data

for (iter in 1:max.iter) {
  sigma2_spatial_old <- sigma2_spatial
  phi_old <- phi
  beta_old <- beta
  u_est_old <- u_est # Initialize u_est_old in the first iteration
  
  # E-Step: Estimate spatial random effects
  cov_matrix <- sigma2_spatial_old * exp(-1 * distances / phi_old) # Exponential decay of correlation with distance
  cov_matrix_inv <- solve(cov_matrix + diag(1e-6, nrow(cov_matrix))) # Add a small value to diagonal for numerical stability
  mu <- X %*% beta_old
  u_est <- cov_matrix_inv %*% (log(data$y + 0.01) - mu)
  
  # M-Step: Update fixed effects
  beta <- solve(t(X) %*% X, t(X) %*% (log(data$y + 0.01) - u_est))
  
  # Check for convergence
  if (iter > 1 && max(abs(u_est - u_est_old), na.rm = TRUE) < tolerance) {
    conv <- TRUE
    cat("Converged in", iter, "iterations\n")
    break
  }
}
beta


#############################################
# MCEM Algorithm (120723)
# 1. Create a spatial GLMM toy dataset
set.seed(123)
n_locations <- 100

## Create two predictors X1 and X2
X1 <- runif(n_locations, -1, 1)
X2 <- rnorm(n_locations, 0, 1)

## True values of the parameters
beta0 <- log(5)
beta1 <- 0.5
beta2 <- -0.3
sigma2_spatial <- 1
phi <- 1
coordinates <- cbind(runif(n_locations, 0, 50), runif(n_locations, 0, 50))

# Create a function to simulate spatial random effects (multivariate normal distribution)
spatial_u <- function(sigma2_spatial, phi, coordinates){
  # Simulate spatial random effect using a Gaussian field
  cov_matrix <- sigma2_spatial * exp(-1 * (as.matrix(dist(coordinates)))/phi) # Exponential decay of correlation with distance
  chol_matrix <- chol(cov_matrix) # Cholesky decomposition
  spatial_effect <- chol_matrix %*% rnorm(n_locations)
  return(spatial_effect)
}
u <- spatial_u(sigma2_spatial=1, phi=1, coordinates)

# Simulate response variable (Poisson-distributed count data)
lambda <- exp(beta0 + beta1 * X1 + beta2 * X2 + u)  # Poisson rate
y <- rpois(n_locations, lambda)
data <- data.frame(y, X1, X2, coordinates)
colnames(data) <- c("y", "X1", "X2", "coord_1", "coord_2")


# Initialize parameters
beta1 <- 0.1
beta2 <- 0.1
u_est <- rep(0, n_locations)
last_mcmc_samples <- rep(0, n_locations)

# EM Algorithm settings
max_iter <- 20
burn_in <- 1000
n_mcmc <- 2000

# Proposal and acceptance functions
# Function to calculate the proposal distribution
proposal_function <- function(current_effect) {
  return(rnorm(1, mean = current_effect, sd = 1))
}

# Function to calculate acceptance probability
acceptance_probability <- function(proposed_effect, current_effect, beta1, beta2, y, X1, X2) {
  # Calculate the likelihood ratio
  proposed_likelihood <- sum(dpois(y, lambda = exp(beta1 * X1 + beta2 * X2 + proposed_effect), log = TRUE))
  current_likelihood <- sum(dpois(y, lambda = exp(beta1 * X1 + beta2 * X2 + current_effect), log = TRUE))
  return(min(1, exp(proposed_likelihood - current_likelihood)))
}

acceptance_probability(3, 1, 0.1, 0.1, data$y, data$X1, data$X2)
proposal_function(1)
data$y
# Start EM Algorithm
for (iter in 1:max_iter) {
  
  # E-Step: Metropolis-Hastings for spatial random effects
  for (loc in 1:n_locations) {
    current_effect <- last_mcmc_samples[loc]
    spatial_effect_sum <- 0 # Reset sum for averaging
    
    for (mcmc_iter in 1:n_mcmc) {
      proposed_effect <- proposal_function(current_effect)
      alpha <- acceptance_probability(proposed_effect, current_effect, beta1, beta2, data$y, data$X1, data$X2)
      if (runif(1) < alpha) {
        current_effect <- proposed_effect
      }
      if (mcmc_iter > burn_in) {
        spatial_effect_sum <- spatial_effect_sum + current_effect
      }
    }
    
    # Update spatial effect estimate and store the last MCMC sample
    u_est[loc] <- spatial_effect_sum / (n_mcmc - burn_in)
    last_mcmc_samples[loc] <- current_effect
  }
  
  # M-Step: Update fixed effect (beta)
  fitted_model <- lm(log(y+1) ~ X1 + X2 + u_est - 1, data = data)
  beta1 <- coef(fitted_model)['X1']
  beta2 <- coef(fitted_model)['X2']
}

# Final Parameter Estimates
print(paste("Beta1:", beta1))
print(paste("Beta2:", beta2))
print("Spatial Effects:")
print(u_est)
mean(u_est)
u
