data {
    int<lower=0> N;  // total number of areas (observations)
    int<lower=0> K; // Number of predictors
    int<lower=0> Y[N];  // outcome variable for each area
    matrix[N, K] X;  // design matrix for fixed effects (standardized predictors)
    int<lower=1> A_sparse[4975, 2]; // sparse representation of adjacency matrix
    matrix[N, N] P_perp; // projection matrix for orthogonalizing spatial random effects
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
    alpha ~ normal(0, 1);
    beta ~ normal(0, 1);
    W ~ normal(0, tau); // Precision parameter for spatial random effects
    
    // ICAR prior for spatial random effects using sparse adjacency matrix
    for (n in 1:4975) {
        int i = A_sparse[n, 1];
        int j = A_sparse[n, 2];
        if (num_neighbors[i] > 0) {
            target += W[i] * W[j];
        }
    }

    // Additional ICAR prior component for each area
    for (i in 1:N) {
        if (num_neighbors[i] > 0) {
            target += -0.5 * num_neighbors[i] * square(W[i]);
        }
    }
  
    // Likelihood
    for (i in 1:N) {
        Y[i] ~ poisson_log(alpha + dot_product(X[i], beta) + W_transformed[i]);
    }
}