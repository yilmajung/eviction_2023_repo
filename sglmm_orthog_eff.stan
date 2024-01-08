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
    int<lower=0, upper=1> A_sparse[N, 2]; // sparse representation of adjacency matrix
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

    // Populate observed and missing values in one loop each
    full_medinc[obs_medinc_idx] = medinc_obs;
    full_medinc[miss_medinc_idx] = medinc_miss;
    full_medrent[obs_medrent_idx] = medrent_obs;
    full_medrent[miss_medrent_idx] = medrent_miss;
    full_medvalue[obs_medvalue_idx] = medvalue_obs;
    full_medvalue[miss_medvalue_idx] = medvalue_miss;
    full_renter_hhsize[obs_renter_hhsize_idx] = renter_hhsize_obs;
    full_renter_hhsize[miss_renter_hhsize_idx] = renter_hhsize_miss;

    // Combine into full design matrix
    matrix[N, K] full_X = append_col(X_other, [full_medinc, full_medrent, full_medvalue, full_renter_hhsize]);

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
    
    // ICAR prior for spatial random effects using sparse adjacency matrix
    for (n in 1:size(A_sparse)[1]) {
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
        Y[i] ~ poisson_log(alpha + full_X[i]*beta + W_transformed[i]);
    }
}