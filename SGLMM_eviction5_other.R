library(tidyverse)
library(rstan)
library(sf)
library(spdep)
library(mice)

#rm(list=ls())
# Load the main data
#df <- read_csv("data/acs_evic_data_wo_zero_pop.csv")
df_np <- read_csv("data/acs_evic_data_np_wo_zero_pop.csv")
df_np_geom <- df_np

# Convert df_geom to SF object using geometry information
df_np_geom <- st_as_sf(df_np_geom, wkt = "geometry_x")

# Read the CBG shapefile
# cbg <- st_read("/Users/wooyongjung/WJ_Projects/eviction_geospatial/data/tl_2022_48_bg/tl_2022_48_bg.shp")

# Create a neighbors list from the census tract BG polygons
neighbors_list <- poly2nb(df_np_geom$geometry_x)

# Create the binary adjacency matrix, with "B" for binary
A <- nb2mat(neighbors_list, style='B', zero.policy=TRUE)
dim(A)

# Create a sparse matrix for A
neighbors <- which(A==1, arr.ind=TRUE)
A_sparse <- matrix(neighbors, ncol=2)
A_sparse <- A_sparse[A_sparse[,1] < A_sparse[,2], ]
dim(A_sparse)

# Standardize predictors
covariates <- c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
                'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
                'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
                'renter_occ_rate', '1unit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

df2 <- df_np[covariates]
# colnames(df2) <- c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
#                    'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
#                    'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
#                    'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
#                    'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
#                    'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')
colnames(df2) <- c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
                'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
                'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
                'renter_occ_rate', 'oneunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')
summary(df2)

# Check missing values pattern
# md.pattern(df2)

# Impute missing values using MICE
imputed <- mice(df2, m=5, maxit=100, method='pmm', seed=123)
df3 <- complete(imputed, 2)
summary(df3)

# Standardize predictors

covariates <- c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
                'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
                'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
                'renter_occ_rate', 'oneunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

df3[covariates] <- scale(df3[covariates])
df3 <- cbind(df_np$case_number, df_np$eviction_rate, df3)
colnames(df3)[1:2] <- c("case_number", "eviction_rate")
dim(df3)

#tempdir()
#dir.create(tempdir())

# Calculate P_perp
# Create a design matrix
X <- as.matrix(df3[covariates])
dim(X)

# Create an identity matrix
I <- diag(nrow(X))

# Compute (X'X)^-1
XX_inv <- solve(t(X) %*% X, tol = 3e-18)

# Compute P_perp
P_perp <- I - X %*% XX_inv %*% t(X)
XX_inv_tx <- XX_inv %*% t(X)
dim(P_perp)
dim(XX_inv_tx)

# Set up the data list for Stan (Y = eviction_rate)
stan_data <- list(N = nrow(df3), 
                  K = 23,
                  A_N = dim(A_sparse)[1],
                  Y = df3$eviction_rate,
                  X = X,
                  A_sparse = A_sparse,
                  P_perp = P_perp,
                  XX_inv_tx = XX_inv_tx,
                  num_neighbors=colSums(A),
                  tau = 10)

# Fit the model
fit <- stan(file = 'sglmm_orthog_eff_small.stan', data = stan_data, 
            iter=10000, chains=4, cores=4, warmup=4000, thin=5,
            control = list(adapt_delta = 0.9, max_treedepth = 15))

summary(fit)
getwd()

# Extract the results
posterior_estimates <- rstan::extract(fit)

stan_trace(fit, pars=c("beta_orig"))
stan_trace(fit, pars=c("beta"))
stan_trace(posterior_estimates$beta_orig[1:10, 1:10])
dim(posterior_estimates$beta_orig)
summary(posterior_estimates$beta_orig)

# Extract the fixed effects coefficients
# c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
#   'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
#   'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
#   'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
#   'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
#   'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')
c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
                'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
                'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
                'renter_occ_rate', 'oneunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

intercept_samples <- posterior_estimates$alpha
rent_mt50_samples <- posterior_estimates$beta_orig[,1]
social_program_samples <- posterior_estimates$beta_orig[,2]
hh_w_child_samples <- posterior_estimates$beta_orig[,3]
edu_grad_samples <- posterior_estimates$beta_orig[,4]
hh_w_child_male_samples <- posterior_estimates$beta_orig[,5]
hh_w_child_female_samples <- posterior_estimates$beta_orig[,6]
unemp_samples <- posterior_estimates$beta_orig[,7]
black_ratio_samples <- posterior_estimates$beta_orig[,8]
hispanic_ratio_samples <- posterior_estimates$beta_orig[,9]
medage_samples <- posterior_estimates$beta_orig[,10]
living_alone_samples <- posterior_estimates$beta_orig[,11]
mort_ratio_samples <- posterior_estimates$beta_orig[,12]
renter_occ_rate_samples <- posterior_estimates$beta_orig[,13]
unit1_structure_samples <- posterior_estimates$beta_orig[,14]
vacancy_rate_samples <- posterior_estimates$beta_orig[,15]
medrent_change_samples <- posterior_estimates$beta_orig[,16]
medvalue_change_samples <- posterior_estimates$beta_orig[,17]
time_to_work_lt30_samples <- posterior_estimates$beta_orig[,18]
time_to_work_mt60_samples <- posterior_estimates$beta_orig[,19]
no_internet_access_samples <- posterior_estimates$beta_orig[,20]
medinc_samples <- posterior_estimates$beta_orig[,21]
medrent_samples <- posterior_estimates$beta_orig[,22]
medvalue_samples <- posterior_estimates$beta_orig[,23]
spatial_effects_samples <- posterior_estimates$W_transformed

dim(posterior_estimates$beta_orig)
dim(posterior_estimates$beta)
dim(intercept_samples)


# Create a data frame for 4000 samples
df_samples <- data_frame(intercept_samples, rent_mt50_samples, social_program_samples,
                         hh_w_child_samples, edu_grad_samples, hh_w_child_male_samples, hh_w_child_female_samples, 
                         unemp_samples, black_ratio_samples, hispanic_ratio_samples,
                         medage_samples, living_alone_samples, mort_ratio_samples, renter_occ_rate_samples, 
                         unit1_structure_samples, vacancy_rate_samples, medrent_change_samples,
                         medvalue_change_samples,
                         time_to_work_lt30_samples, time_to_work_mt60_samples, no_internet_access_samples,
                         medinc_samples, medrent_samples, medvalue_samples, spatial_effects_samples)
dim(df_samples)
write.csv(df_samples, "data/results/df_samples_other_final_2.csv")

df_95ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.025, 0.975))))
df_mean <- data_frame(sapply(df_samples, function(x) mean(x)))
dim(df_95ci)
dim(df_mean)
df_95ci <- cbind(df_95ci, df_mean)
View(df_95ci)

#########################
# # Add original beta values

# intercept_samples <- posterior_estimates$alpha
# poverty_samples <- posterior_estimates$beta_orig[,1]
# rent_mt40_samples <- posterior_estimates$beta_orig[,2]
# social_program_samples <- posterior_estimates$beta_orig[,3]
# hh_w_child_samples <- posterior_estimates$beta_orig[,4]
# unemp_samples <- posterior_estimates$beta_orig[,5]
# black_ratio_samples <- posterior_estimates$beta_orig[,6]
# white_ratio_samples <- posterior_estimates$beta_orig[,7]
# asian_ratio_samples <- posterior_estimates$beta_orig[,8]
# hispanic_ratio_samples <- posterior_estimates$beta_orig[,9]
# edu_lt_hs_samples <- posterior_estimates$beta_orig[,10]
# medage_samples <- posterior_estimates$beta_orig[,11]
# nonfam_samples <- posterior_estimates$beta_orig[,12]
# renter_occ_rate_samples <- posterior_estimates$beta_orig[,13]
# mort_ratio_samples <- posterior_estimates$beta_orig[,14]
# unit1_structure_samples <- posterior_estimates$beta_orig[,15]
# vacancy_rate_samples <- posterior_estimates$beta_orig[,16]
# medrent_change_samples <- posterior_estimates$beta_orig[,17]
# time_to_work_lt30_samples <- posterior_estimates$beta_orig[,18]
# time_to_work_30to59_samples <- posterior_estimates$beta_orig[,19]
# time_to_work_mt60_samples <- posterior_estimates$beta_orig[,20]
# medinc_samples <- posterior_estimates$beta_orig[,21]
# medrent_samples <- posterior_estimates$beta_orig[,22]
# medvalue_samples <- posterior_estimates$beta_orig[,23]
# renter_hhsize_samples <- posterior_estimates$beta_orig[,24]
# spatial_effects_samples <- posterior_estimates$W_transformed

# df_samples_orig <- data_frame(intercept_samples, poverty_samples, rent_mt40_samples, social_program_samples,
#                               hh_w_child_samples, unemp_samples, black_ratio_samples, white_ratio_samples,
#                               asian_ratio_samples, hispanic_ratio_samples, edu_lt_hs_samples, 
#                               medage_samples, nonfam_samples, renter_occ_rate_samples, mort_ratio_samples, 
#                               unit1_structure_samples, vacancy_rate_samples, medrent_change_samples,
#                               time_to_work_lt30_samples, time_to_work_30to59_samples, time_to_work_mt60_samples,
#                               medinc_samples, medrent_samples, medvalue_samples, renter_hhsize_samples, spatial_effects_samples)

# df_95ci_orig <- t(sapply(df_samples_orig, function(x) quantile(x, probs = c(0.025, 0.975))))
# df_mean_orig <- data_frame(sapply(df_samples_orig, function(x) mean(x)))
# dim(df_95ci_orig)
# dim(df_mean_orig)
# df_95ci_orig <- cbind(df_95ci_orig, df_mean_orig)
# df_95ci <- cbind(df_95ci, df_95ci_orig)
# View(df_95ci)
################################

# 90% CI
df_90ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.05, 0.95))))
df_95ci <- cbind(df_95ci, df_90ci)

# Save the results
write.csv(df_95ci, "data/results/df_95ci_other_final_2.csv")
View(df_95ci)

# Extract the spatial random effects
# Load removed CBGs
df_removed <- read_csv('data/eviction_count_bg_2021_for_removed_cbg.csv')

# Add spatial effects to df_np
avg_spatial_effects <- apply(spatial_effects_samples, 2, mean)
df_np$spatial_effect <- avg_spatial_effects

# Merge df_np and df_removed
df_np <- merge(df_np, df_removed, by=c('GEOID', 'geometry_x'), all.y=TRUE)

# Convert df_np to SF object
df_np <- st_as_sf(df_np, wkt = "geometry_x")
write.csv(df_np, 'data/results/df_geom_other_final_2.csv')

plot1 <- ggplot(df_np) + 
        geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) + 
        coord_sf(datum=st_crs(3857)) +
        scale_fill_viridis_c() + 
        labs(title="Spatial Random Effects", fill="Spatial Effect") + 
        theme_bw()
ggplot(df_np) + geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) + scale_fill_viridis_c() +
theme_bw()

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) +
  scale_fill_viridis_c() +
  labs(title="Spatial Random Effects", fill="Effect") +
  theme_minimal()

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect), color=NA) +
  scale_fill_gradient2(midpoint=0, low='blue', mid='yellow', high='red', space='Lab') +
  labs(title="Spatial Random Effects", fill="Effect") +
  theme_minimal()

ggplot(df_geom) +
  geom_sf(aes(fill=case_number), color=NA) +
  scale_fill_viridis_c(option="plasma") +
  labs(title="Number of Eviction Filings", fill="# of Eviction") +
  theme_minimal()


# Diagnostics after fitting
# Visualization with bayesplot package
library(bayesplot)
available_mcmc(pattern='_nuts_')
log_posterior(fit)
mcmc_parcoord(fit, pars=c("beta_orig[1]", "beta_orig[2]"))

dim(as.data.frame(fit))
df_fit <- as.data.frame(fit)
mcmc_hist(df_fit, pars=c("beta"))
head(df_fit)
mcmc_dens(df_fit, pars=c('beta'))
?mcmc_hist
mcmc_hist()
mcmc_intervals(df_fit, pars=c('alpha'))
mcmc_intervals(df_fit, pars=c('beta_orig[1]','beta_orig[2]'), prob=0.95)
packageVersion("bayesplot")
