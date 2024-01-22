library(tidyverse)
library(rstan)
library(sf)
library(spdep)

rm(list=ls())
# Load the main data
df <- read_csv("data/acs_evic_data_wo_zero_pop.csv")
#df_np <- read_csv("data/acs_evic_data_np_wo_zero_pop.csv")

df_geom <- df
#df_np_geom <- df_np

# Convert df_geom to SF object using geometry information
df_geom <- st_as_sf(df_geom, wkt = "geometry_x")
#df_np_geom <- st_as_sf(df_np_geom, wkt = "geometry_x")

# Read the CBG shapefile
# cbg <- st_read("/Users/wooyongjung/WJ_Projects/eviction_geospatial/data/tl_2022_48_bg/tl_2022_48_bg.shp")

# Create a neighbors list from the census tract BG polygons
neighbors_list <- poly2nb(df_geom$geometry_x)

# Create the binary adjacency matrix, with "B" for binary
A <- nb2mat(neighbors_list, style='B', zero.policy=TRUE)
dim(A)

# Create a sparse matrix for A
neighbors <- which(A==1, arr.ind=TRUE)
A_sparse <- matrix(neighbors, ncol=2)
A_sparse <- A_sparse[A_sparse[,1] < A_sparse[,2], ]
dim(A_sparse)

# Standardize predictors
covariates <- c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
                'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
                'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
                'renter_occ_rate', 'mortgage_status_ratio', '1unit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
                'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')


df2 <- df[covariates]
colnames(df2) <- c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
                   'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
                   'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
                   'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
                   'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
                   'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')
summary(df2)

# Check missing values pattern
md.pattern(df2)

# Impute missing values using MICE
imputed <- mice(df2, m=5, maxit=100, method='pmm', seed=123)
df3 <- complete(imputed, 2)
summary(df3)

# Standardize predictors

covariates <- c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
                'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
                'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
                'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
                'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')


df3[covariates] <- scale(df3[covariates])
df3 <- cbind(df$case_number, df$eviction_rate, df3)
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
P_perp[1:10, 1:10]
dim(A_sparse)[1]
# Set up the data list for Stan (Y = eviction_rate)
stan_data <- list(N = nrow(df3), 
                  K = 24,
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
c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
  'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
  'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
  'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
  'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
  'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')

intercept_samples <- posterior_estimates$alpha
poverty_samples <- posterior_estimates$beta[,1]
rent_mt40_samples <- posterior_estimates$beta[,2]
social_program_samples <- posterior_estimates$beta[,3]
hh_w_child_samples <- posterior_estimates$beta[,4]
unemp_samples <- posterior_estimates$beta[,5]
black_ratio_samples <- posterior_estimates$beta[,6]
white_ratio_samples <- posterior_estimates$beta[,7]
asian_ratio_samples <- posterior_estimates$beta[,8]
hispanic_ratio_samples <- posterior_estimates$beta[,9]
edu_lt_hs_samples <- posterior_estimates$beta[,10]
medage_samples <- posterior_estimates$beta[,11]
nonfam_samples <- posterior_estimates$beta[,12]
renter_occ_rate_samples <- posterior_estimates$beta[,13]
mort_ratio_samples <- posterior_estimates$beta[,14]
unit1_structure_samples <- posterior_estimates$beta[,15]
vacancy_rate_samples <- posterior_estimates$beta[,16]
medrent_change_samples <- posterior_estimates$beta[,17]
time_to_work_lt30_samples <- posterior_estimates$beta[,18]
time_to_work_30to59_samples <- posterior_estimates$beta[,19]
time_to_work_mt60_samples <- posterior_estimates$beta[,20]
medinc_samples <- posterior_estimates$beta[,21]
medrent_samples <- posterior_estimates$beta[,22]
medvalue_samples <- posterior_estimates$beta[,23]
renter_hhsize_samples <- posterior_estimates$beta[,24]
spatial_effects_samples <- posterior_estimates$W_transformed

dim(posterior_estimates$beta_orig)
dim(posterior_estimates$beta)
dim(intercept_samples)


# Create a data frame for 4000 samples
df_samples <- data_frame(intercept_samples, poverty_samples, rent_mt40_samples, social_program_samples,
                         hh_w_child_samples, unemp_samples, black_ratio_samples, white_ratio_samples,
                         asian_ratio_samples, hispanic_ratio_samples, edu_lt_hs_samples, 
                         medage_samples, nonfam_samples, renter_occ_rate_samples, mort_ratio_samples, 
                         unit1_structure_samples, vacancy_rate_samples, medrent_change_samples,
                         time_to_work_lt30_samples, time_to_work_30to59_samples, time_to_work_mt60_samples,
                         medinc_samples, medrent_samples, medvalue_samples, renter_hhsize_samples, spatial_effects_samples)

df_95ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.025, 0.975))))
df_mean <- data_frame(sapply(df_samples, function(x) mean(x)))
dim(df_95ci)
dim(df_mean)
df_95ci <- cbind(df_95ci, df_mean)
View(df_95ci)

#########################
# Add original beta values

intercept_samples <- posterior_estimates$alpha
poverty_samples <- posterior_estimates$beta_orig[,1]
rent_mt40_samples <- posterior_estimates$beta_orig[,2]
social_program_samples <- posterior_estimates$beta_orig[,3]
hh_w_child_samples <- posterior_estimates$beta_orig[,4]
unemp_samples <- posterior_estimates$beta_orig[,5]
black_ratio_samples <- posterior_estimates$beta_orig[,6]
white_ratio_samples <- posterior_estimates$beta_orig[,7]
asian_ratio_samples <- posterior_estimates$beta_orig[,8]
hispanic_ratio_samples <- posterior_estimates$beta_orig[,9]
edu_lt_hs_samples <- posterior_estimates$beta_orig[,10]
medage_samples <- posterior_estimates$beta_orig[,11]
nonfam_samples <- posterior_estimates$beta_orig[,12]
renter_occ_rate_samples <- posterior_estimates$beta_orig[,13]
mort_ratio_samples <- posterior_estimates$beta_orig[,14]
unit1_structure_samples <- posterior_estimates$beta_orig[,15]
vacancy_rate_samples <- posterior_estimates$beta_orig[,16]
medrent_change_samples <- posterior_estimates$beta_orig[,17]
time_to_work_lt30_samples <- posterior_estimates$beta_orig[,18]
time_to_work_30to59_samples <- posterior_estimates$beta_orig[,19]
time_to_work_mt60_samples <- posterior_estimates$beta_orig[,20]
medinc_samples <- posterior_estimates$beta_orig[,21]
medrent_samples <- posterior_estimates$beta_orig[,22]
medvalue_samples <- posterior_estimates$beta_orig[,23]
renter_hhsize_samples <- posterior_estimates$beta_orig[,24]
spatial_effects_samples <- posterior_estimates$W_transformed

df_samples_orig <- data_frame(intercept_samples, poverty_samples, rent_mt40_samples, social_program_samples,
                              hh_w_child_samples, unemp_samples, black_ratio_samples, white_ratio_samples,
                              asian_ratio_samples, hispanic_ratio_samples, edu_lt_hs_samples, 
                              medage_samples, nonfam_samples, renter_occ_rate_samples, mort_ratio_samples, 
                              unit1_structure_samples, vacancy_rate_samples, medrent_change_samples,
                              time_to_work_lt30_samples, time_to_work_30to59_samples, time_to_work_mt60_samples,
                              medinc_samples, medrent_samples, medvalue_samples, renter_hhsize_samples, spatial_effects_samples)

write.csv(df_samples_orig, "df_samples_orig_final.csv")
df_95ci_orig <- t(sapply(df_samples_orig, function(x) quantile(x, probs = c(0.025, 0.975))))
df_mean_orig <- data_frame(sapply(df_samples_orig, function(x) mean(x)))
dim(df_95ci_orig)
dim(df_mean_orig)
df_95ci_orig <- cbind(df_95ci_orig, df_mean_orig)
df_95ci <- cbind(df_95ci, df_95ci_orig)

df_90ci_orig <- t(sapply(df_samples_orig, function(x) quantile(x, probs = c(0.05, 0.95))))
df_95ci <- cbind(df_95ci, df_90ci_orig)
write.csv(df_95ci, "df_95ci_final_nonpayment.csv")

################################


# Extract the spatial random effects
spatial_effects <- posterior_estimates$W_transformed
dim(spatial_effects)
avg_spatial_effects <- apply(spatial_effects, 2, mean)
df_geom$spatial_effect <- avg_spatial_effects
write.csv(df_geom, 'df_geom_final_nonpayment.csv')

class(df_geom)

plot1 <- ggplot(df_geom) + geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) + 
  scale_fill_viridis_c() + labs(title="Spatial Random Effects", fill="Effect") + theme_minimal()


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
