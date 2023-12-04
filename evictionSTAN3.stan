data {
  int<lower=0> N;  // total number of areas (observations)
  int<lower=0> N_obs_medinc; // Number of observed values in household median income
  int<lower=0> N_miss_medinc; // Number of missing values in household median income
  int<lower=0> Y[N];  // outcome variable for each area
  vector[N_obs_medinc] medinc_obs; // Observed values of household median income
  int<lower=0, upper=N> obs_medinc_idx[N_obs_medinc]; // Indices of observed values in household median income
  int<lower=0, upper=N> miss_medinc_idx[N_miss_medinc]; // Indices of missing values in household median income
  matrix[N, 3] X_other;  // design matrix for fixed effects (standardized predictors)
  matrix<lower=0, upper=1>[N, N] W;  // adjacency matrix, 0 or 1
  int<lower=0> num_neighbors[N];  // number of neighbors for each area
  real<lower=0> tau;  // precision parameter for spatial effects
}

parameters {
  real alpha;  // intercept
  vector[4] beta;  // coefficients for predictors
  vector[N_miss_medinc] medinc_miss; // missing values of medinc
  real<lower=0> sigma_medinc; // standard deviation for the distribution of medinc
  vector[N] phi;  // spatial random effects
}

model {
  vector[N] medinc; // full vector for medinc including imputed values
  
  // Priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma_medinc ~ normal(0, 1);
  
  // ICAR prior for spatial random effects
  for (i in 1:N) {
    if (num_neighbors[i] > 0) {
      target += -0.5 * num_neighbors[i] * square(phi[i]);
      for (j in 1:N) {
        if (W[i, j] == 1) {
          target += phi[i] * phi[j];
        }
      }
    }
  }
  
  // Precision parameter for spatial random effects
  phi ~ normal(0, tau);
  
  // Model for observed values of medinc
  medinc_obs ~ normal(0, sigma_medinc);
  
  // Impute misssing values of medinc
  medinc_miss ~ normal(0, sigma_medinc);
  
  // Reconstruct the full medinc vector
  medinc[obs_medinc_idx] = medinc_obs;
  medinc[miss_medinc_idx] = medinc_miss;

  // Likelihood
  for (i in 1:N) {
    Y[i] ~ poisson_log(alpha + beta[1]*medinc[i] + dot_product(X_other[i], beta[2:4]) + phi[i]);
  }