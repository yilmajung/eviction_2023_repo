data {
  int<lower=0> N;  // number of areas
  int<lower=0> Y[N];  // outcome variable for each area
  matrix[N, 4] X;  // design matrix for fixed effects (standardized predictors)
  matrix<lower=0, upper=1>[N, N] W;  // adjacency matrix, 0 or 1
  int<lower=0> num_neighbors[N];  // number of neighbors for each area
  real<lower=0> tau;  // precision parameter for spatial effects
}

parameters {
  real alpha;  // intercept
  vector[4] beta;  // coefficients for predictors
  vector[N] phi;  // spatial random effects
}

model {
  // Priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  
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

  // Likelihood
  for (i in 1:N) {
    Y[i] ~ poisson_log(alpha + dot_product(X[i], beta) + phi[i]);
  }
}