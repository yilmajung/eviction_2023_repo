data {
  int<lower=0> N;  // number of areas
  int<lower=0> Y[N];  // count data for each area
  matrix[N, N] W;  // adjacency matrix
  matrix[N, 4] X;  // matrix of covariates
  real<lower=0> alpha_sd;  // prior std for the intercept
  real<lower=0> beta_sd;  // prior std for the covariates
  real<lower=0> tau_sd;  // prior std for the spatial component
}

parameters {
  real alpha;  // intercept
  vector[4] beta;  // coefficients for covariates
  vector[N] phi;  // spatial effects
  real<lower=0> sigma;  // standard deviation of spatial effects
}

model {
  vector[N] mu;
  
  // Priors
  alpha ~ normal(0, alpha_sd);
  beta ~ normal(0, beta_sd);
  sigma ~ cauchy(0, tau_sd);
  
  // BYM model for spatial effects
  phi ~ multi_normal_prec(rep_vector(0, N), sigma * (diag_matrix(rep_vector(1, N)) - W));

  // Poisson model for data
  for (n in 1:N) {
    mu[n] = exp(alpha + dot_product(X[n], beta) + phi[n]);
    Y[n] ~ poisson(mu[n]);
  }
}
