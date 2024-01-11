data {
    int<lower=0> N;  // total number of areas (observations)
    int<lower=0> K; // Number of predictors
    int<lower=0> Y[N];  // outcome variable for each area
    matrix[N, K] X;  // design matrix for fixed effects (standardized predictors)
    matrix<lower=0, upper=1>[N, N] A;  // adjacency matrix, 0 or 1
    matrix[N, N] P_perp; // projection matrix for orthogonalizing spatial random effects
    matrix[K, N] XX_inv_tx; // projection matrix for calculating original betas
    int<lower=0> num_neighbors[N];  // number of neighbors for each area
    real<lower=0> tau;  // precision parameter for spatial effects
}

parameters {
    real alpha;  // intercept
    vector[K] beta;  // coefficients for predictors
    vector[N] W;  // spatial random effects
}

transformed parameters {
    vector[N] W_transformed = P_perp * W; // Apply the orthogonal projection to W
}

model {
    // Priors
    alpha ~ normal(0, 30);
    beta ~ normal(0, 5);
    W ~ normal(0, tau); // Precision parameter for spatial random effects
    
    // ICAR prior for spatial random effects
    for (i in 1:N) {
        if (num_neighbors[i] > 0) {
        target += -0.5 * num_neighbors[i] * square(W[i]);
        for (j in 1:N) {
            if (A[i, j] == 1) {
            target += W[i] * W[j];
            }
        }
        }
    }
  
    // Likelihood
    for (i in 1:N) {
        Y[i] ~ poisson_log(alpha + dot_product(X[i], beta) + W_transformed[i]);
    }
}

generated quantities {
    vector[K] beta_orig = beta - (XX_inv_tx * W);  // coefficients for predictors
}