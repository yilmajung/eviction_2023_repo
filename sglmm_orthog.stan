data {
    int<lower=0> N;  // total number of areas (observations)
    int<lower=0> K; // Number of predictors
    int<lower=0> N_obs_medinc; // Number of observed values in household median income
    int<lower=0> N_miss_medinc; // Number of missing values in household median income
    int<lower=0> N_obs_medrent; // Number of observed values in median gross rent
    int<lower=0> N_miss_medrent; // Number of missing values in median gross rent
    int<lower=0> N_obs_medvalue; // Number of observed values in housing median value
    int<lower=0> N_miss_medvalue; // Number of missing values in housing median value
    int<lower=0> N_obs_renter_hhsize; // Number of observed values in hh avg size (renter occupied)
    int<lower=0> N_miss_renter_hhsize; // Number of missing values in hh avg size (renter occupied)
    int<lower=0> Y[N];  // outcome variable for each area
    vector[N_obs_medinc] medinc_obs; // Observed values of household median income
    int<lower=0, upper=N> obs_medinc_idx[N_obs_medinc]; // Indices of observed values in household median income
    int<lower=0, upper=N> miss_medinc_idx[N_miss_medinc]; // Indices of missing values in household median income
    vector[N_obs_medrent] medrent_obs; // Observed values of median gross rent
    int<lower=0, upper=N> obs_medrent_idx[N_obs_medrent]; // Indices of observed values in median gross rent
    int<lower=0, upper=N> miss_medrent_idx[N_miss_medrent]; // Indices of missing values in median gross rent
    vector[N_obs_medvalue] medvalue_obs; // Observed values of housing median value
    int<lower=0, upper=N> obs_medvalue_idx[N_obs_medvalue]; // Indices of observed values in housing median value
    int<lower=0, upper=N> miss_medvalue_idx[N_miss_medvalue]; // Indices of missing values in housing median value
    vector[N_obs_renter_hhsize] renter_hhsize_obs; // Observed values of hh avg size (renter)
    int<lower=0, upper=N> obs_renter_hhsize_idx[N_obs_renter_hhsize]; // Indices of observed values in hh avg size (renter)
    int<lower=0, upper=N> miss_renter_hhsize_idx[N_miss_renter_hhsize]; // Indices of missing values in hh avg size (renter)
    matrix[N, K-4] X_other;  // design matrix for fixed effects (standardized predictors)
    matrix<lower=0, upper=1>[N, N] A;  // adjacency matrix, 0 or 1
    int<lower=0> num_neighbors[N];  // number of neighbors for each area
    real<lower=0> tau;  // precision parameter for spatial effects
}

parameters {
    real alpha;  // intercept
    vector[K] beta;  // coefficients for predictors
    vector[N_miss_medinc] medinc_miss; // missing values of medinc
    vector[N_miss_medrent] medrent_miss; // missing values of median gross rent
    vector[N_miss_medvalue] medvalue_miss; // missing values of housing median value
    vector[N_miss_renter_hhsize] renter_hhsize_miss; // missing values of avg hh size (renter)
    real<lower=0> sigma_medinc; // standard deviation for the distribution of medinc
    real<lower=0> sigma_medrent; // standard deviation for the distribution of medrent
    real<lower=0> sigma_medvalue; // standard deviation for the distribution of medvalue
    real<lower=0> sigma_renter_hhsize; // standard deviation for the distribution of renter_hhsize
    vector[N] W;  // spatial random effects
}

transformed parameters {
    vector[N] full_medinc; // full vector for medinc including imputed values
    vector[N] full_medrent; // full vector for medrent including imputed values
    vector[N] full_medvalue; // full vector for medvalue including imputed values
    vector[N] full_renter_hhsize; // full vector for renter_hhsize including imputed values
    matrix[N, 24] full_X; // Full design matrix including imputed values

    // Initialize full vectors for each predictor
    full_medinc = rep_vector(0, N);
    full_medrent = rep_vector(0, N);
    full_medvalue = rep_vector(0, N);
    full_renter_hhsize = rep_vector(0, N);

    // Populate observed values
    for (i in 1:N_obs_medinc) {
        full_medinc[obs_medinc_idx[i]] = medinc_obs[i];
    }
    for (i in 1:N_obs_medrent) {
        full_medrent[obs_medrent_idx[i]] = medrent_obs[i];
    }
    for (i in 1:N_obs_medvalue) {
        full_medvalue[obs_medvalue_idx[i]] = medvalue_obs[i];
    }
    for (i in 1:N_obs_renter_hhsize) {
        full_renter_hhsize[obs_renter_hhsize_idx[i]] = renter_hhsize_obs[i];
    }

    // Populate missing values
    for (i in 1:N_miss_medinc) {
        full_medinc[miss_medinc_idx[i]] = medinc_miss[i];
    }
    for (i in 1:N_miss_medrent) {
        full_medrent[miss_medrent_idx[i]] = medrent_miss[i];
    }
    for (i in 1:N_miss_medvalue) {
        full_medvalue[miss_medvalue_idx[i]] = medvalue_miss[i];
    }
    for (i in 1:N_miss_renter_hhsize) {
        full_renter_hhsize[miss_renter_hhsize_idx[i]] = renter_hhsize_miss[i];
    }

    // Combine into full design matrix
    full_X = append_col(X_other, full_medinc);
    full_X = append_col(full_X, full_medrent);
    full_X = append_col(full_X, full_medvalue);
    full_X = append_col(full_X, full_renter_hhsize);

    // Compute the orthogonal projection matrix
    matrix[N, N] I = identity_matrix(N);
    matrix[N, N] P_perp = I - full_X * inverse(full_X' * full_X) * full_X';

    // Apply the orthogonal projection to W
    vector[N] W_transformed = P_perp * W;
}

model {
    // Priors
    alpha ~ normal(0, 1);
    beta ~ normal(0, 1);
    sigma_medinc ~ normal(0, 1);
    sigma_medrent ~ normal(0, 1);
    sigma_medvalue ~ normal(0, 1);
    sigma_renter_hhsize ~ normal(0, 1);
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
  
    // Model for observed values
    medinc_obs ~ normal(0, sigma_medinc);
    medrent_obs ~ normal(0, sigma_medrent);
    medvalue_obs ~ normal(0, sigma_medvalue);
    renter_hhsize_obs ~ normal(0, sigma_renter_hhsize);
    
    // Impute misssing values
    medinc_miss ~ normal(0, sigma_medinc);
    medrent_miss ~ normal(0, sigma_medrent);
    medvalue_miss ~ normal(0, sigma_medvalue);
    renter_hhsize_miss ~ normal(0, sigma_renter_hhsize);

    // Likelihood
    for (i in 1:N) {
        Y[i] ~ poisson_log(alpha + beta[1]*medinc[i] + beta[2]*medrent[i] + beta[3]*medvalue[i] + beta[4]*renter_hhsize[i] + dot_product(X_other[i], beta[5:24]) + W_transformed[i]);
    }
}