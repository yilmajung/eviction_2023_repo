# EM Algorigh practice

set.seed(123)

# Generate some data from two normal distributions
data1 <- rnorm(10, mean=3, sd=1)
data2 <- rnorm(10, mean=7, sd=2)
data <- c(data1, data2)

# Initialize parameters
mu1 <- 1
mu2 <- 10
sigma1 <- 1
sigma2 <- 1
lambda <- 0.5 # Initial guess for the mixing probability

em_alg <- function(data, mu1, mu2, sigma1, sigma2, lambda, n_iter=1000) {
  n <- length(data)
  
  for (i in 1:n_iter) {
    # E-step
    resp1 <- lambda * dnorm(data, mean=mu1, sd=sigma1)
    resp2 <- (1-lambda) * dnorm(data, mean=mu2, sd=sigma2)
    W <- resp1 / (resp1 + resp2)
    
    # M-step
    mu1 <- sum(W * data) / sum(W)
    mu2 <- sum((1-W) * data) / sum(1-W)
    sigma1 <- sqrt(sum(W * (data-mu1)^2) / sum(W))
    sigma2 <- sqrt(sum((1-W) * (data-mu2)^2) / sum(1-W))
    lambda <- mean(W)
  }
  
  list(mu1 = mu1, mu2 = mu2, sigma1 = sigma1, sigma2 = sigma2, lambda = lambda)
}
dnor


################
a <- lambda * dnorm(data, mean=mu1, sd=sigma1)
b <- (1-lambda) * dnorm(data, mean=mu2, sd=sigma2)
c <- a / (a+b)
a[12] + b[12]
sum(c)
sum(c * data) / sum(c)
################


# Run the EM algorithm
result <- em_alg(data, mu1, mu2, sigma1, sigma2, lambda)
print(result)



###############################
# EM algorithm practice 2 (GLMMs)
# Model Specification: Response Variable: Number of doctor visits (Poisson-distributed)
## Fixed Effect: Age of the patient
## Random Effect: Clinic
library(Matrix)
library(lme4)
#install.packages('statmod')
library(statmod)

# Data simulation
set.seed(123)
age <- rnorm(100, 50, 10)
clinic <- factor(rep(1:10, each=10))
visits <- rpois(100, exp(0.04 * age + rnorm(10, 0, 0.5)[clinic]))

# Model Data
data <- data.frame(visits, age, clinic)
Z <- model.matrix(~ clinic - 1, data) # Random effects design matrix
X <- model.matrix(~ age, data) # Fixed effects design matrix

# Initialize parameters
beta <- rep(0, ncol(X)) # Fixed effects
sigma2 <- 1 # Variance of random effects
u <- rep(0, ncol(Z)) # Random effects

# Convergence settings
max.iter <- 1000
tolerance <- 1e-8
conv <- FALSE

# EM Algorithm
for (iter in 1:max.iter) {
  # E-Step: Estimate random effects
  u_old <- u
  
  # Adjust log(visits) to handle zeros
  adjusted_log_visits <- log(visits + 1)
  
  # Random effects estimation
  u <- sigma2 * (solve(crossprod(Z) + sigma2 * Diagonal(ncol(Z)), t(Z) %*% (adjusted_log_visits - X %*% beta)))
  
  # M-Step: Update fixed effects
  beta <- solve(crossprod(X), t(X) %*% (adjusted_log_visits - Z %*% u))
  
  # Check for convergence
  if (max(abs(u - u_old), na.rm = TRUE) < tolerance) {
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

cat("Random Effects (u):\n")
print(u)

a <- rbind(c(1, 2), c(3, 4))
crossprod(a)
t(a)%*%a
Z
D
Z %*% Diagonal(ncol(Z)) %*% t(Z)


# Another example
set.seed(123)
n_patients <- 200
n_clinics <- 5
n_doctors_per_clinic <- 5
n_doctors <- n_clinics * n_doctors_per_clinic

# Simulate patient data
age <- rnorm(n_patients, 50, 10)
chronic_condition <- rbinom(n_patients, 1, 0.3)

# Assign patients to clinics and doctors
clinic <- rep(1:n_clinics, each = n_patients / n_clinics)
doctor <- rep(rep(1:n_doctors_per_clinic, each=n_patients/n_doctors), n_clinics)

# Simulate random effects
clinic_effect <- rnorm(n_clinics, 0, 0.5)
doctor_effect <- rnorm(n_doctors, 0, 0.3)

# Simulate response variable
lambda <- exp(0.02*age - 0.5*chronic_condition + clinic_effect[clinic] + doctor_effect[doctor])
service_uses <- rpois(n_patients, lambda)

# Create data frame
data <- data.frame(service_uses, age, chronic_condition, clinic, doctor)

# Design matrices for fixed and random effects
X <- model.matrix(~ age + chronic_condition, data = data) # Fixed effect
Z_clinic <- model.matrix(~ clinic - 1, data = data) # Random effect for clinic
Z_doctor <- model.matrix(~ doctor - 1, data = data) # Random effect for doctor

# Initialize parameters
beta <- rep(0, ncol(X)) # Fixed effects
sigma2_clinic <- 1 # Variance for clinic random effects
sigma2_doctor <- 1 # Variance for doctor random effects
u_clinic <- rep(0, ncol(Z_clinic)) # Random effect for clinic
u_doctor <- rep(0, ncol(Z_doctor)) # Random effect for doctor

# Convergence settings
max.iter <- 1000
tolerance <- 1e-8
conv <- FALSE

# EM Algorithm
for (iter in 1:max.iter) {
  # E-Step: Estimate random effects for clinic and doctor
  u_clinic_old <- u_clinic
  u_doctor_old <- u_doctor
  
  u_clinic <- sigma2_clinic * (solve(crossprod(Z_clinic) + sigma2_clinic * Diagonal(ncol(Z_clinic)), t(Z_clinic) %*% (log(data$service_uses + 1) - X %*% beta - Z_doctor %*% u_doctor)))
  u_doctor <- sigma2_doctor * (solve(crossprod(Z_doctor) + sigma2_doctor * Diagonal(ncol(Z_doctor)), t(Z_doctor) %*% (log(data$service_uses + 1) - X %*% beta - Z_clinic %*% u_clinic)))
  
  # M-Step: Update fixed effects
  beta <- solve(crossprod(X), t(X) %*% (log(data$service_uses + 1) - Z_clinic %*% u_clinic - Z_doctor %*% u_doctor))
  
  # Check for convergence
  if (max(abs(u_clinic - u_clinic_old)) < tolerance && max(abs(u_doctor - u_doctor_old)) < tolerance) {
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

cat("Random Effects (Clinic):\n")
print(u_clinic)

cat("Random Effects (Doctor):\n")
print(u_doctor)


# Spatial GLMM toy example
set.seed(123)
n_locations <- 100

# Simulate fixed effects data
altitude <- runif(n_locations, 1000, 3000) # Altitude in meters
temp <- runif(n_locations, 5, 20) # Average temperature in Celsius

# Simulate spatial coordinates
coordinates <- cbind(runif(n_locations, 0, 100), runif(n_locations, 0, 100))

# Simulate spatial random effect using a Gaussian field
library(fields)
cov_matrix <- exp(-0.01 * as.matrix(dist(coordinates))) # Exponential decay of correlation with distance
chol_matrix <- chol(cov_matrix) # Cholesky decomposition
spatial_effect <- chol_matrix %*% rnorm(n_locations)

# Simulate response variable (plant count)
lambda <- exp(0.001 * altitude - 0.1 * temp + spatial_effect)
plant_count <- rpois(n_locations, lambda)

# Create data frame
data <- data.frame(plant_count, altitude, temp, coordinates)
data$X1[1]
coordinates[1,1]

# Assuming 'data' contains the simulated dataset with plant_count, altitude, temp, and coordinates

# Load necessary libraries
library(Matrix)
library(lme4)

# Design Matrices for Fixed Effects
X <- model.matrix(~ altitude + temp, data = data)  # Fixed effects

# Initialize Parameters
beta <- rep(0, ncol(X))                  # Fixed effects
sigma2_spatial <- 1                      # Variance for spatial random effects
n_locations <- nrow(data)
spatial_effect <- rep(0, n_locations)    # Spatial random effects

# Convergence Settings
max.iter <- 3000
tolerance <- 1e-8
conv <- FALSE
data
# EM Algorithm
for (iter in 1:max.iter) {
  # E-Step: Estimate spatial random effects
  spatial_effect_old <- spatial_effect
  
  # Example approach: using inverse distance weighting for spatial effect estimation
  # This is a simplification and should be adapted to your specific spatial model
  for (i in 1:n_locations) {
    distances <- sqrt((data$X1[i] - data$X1)^2 + (data$X2[i] - data$X2)^2)
    weights <- 1 / (distances + 1e-6)  # Adding a small constant to avoid division by zero
    weights[i] <- 0  # Exclude self-distance
    spatial_effect[i] <- sum(weights * (log(data$plant_count + 1) - X[i,] %*% beta)) / sum(weights)
  }
  
  # M-Step: Update fixed effects
  beta <- solve(crossprod(X), t(X) %*% (log(data$plant_count + 1) - spatial_effect))
  
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
library(mvtnorm)

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
