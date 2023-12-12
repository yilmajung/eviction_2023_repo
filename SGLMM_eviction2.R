library(tidyverse)
library(rstan)
library(sf)
#install.packages('spdep')
library(spdep)
library(readr)

# Load the main data
df <- read_csv("data/acs_evic_data_wo_zero_pop.csv")
df_np <- read_csv("data/acs_evic_data_np_wo_zero_pop.csv")

df_geom <- df
df_np_geom <- df_np

# Convert df_geom to SF object using geometry information
df_geom <- st_as_sf(df_geom, wkt = "geometry_x")
df_np_geom <- st_as_sf(df_np_geom, wkt = "geometry_x")

# Read the CBG shapefile
# cbg <- st_read("/Users/wooyongjung/WJ_Projects/eviction_geospatial/data/tl_2022_48_bg/tl_2022_48_bg.shp")

# Create a neighbors list from the census tract BG polygons
neighbors_list <- poly2nb(df_geom$geometry_x)

# Create the binary adjacency matrix, with "B" for binary
W <- nb2mat(neighbors_list, style='B', zero.policy=TRUE)
dim(W)

# Economic Characteristics
'poverty_rate', 'hh_median_income', 'median_gross_rent', 'gross_rent_mt4', 'hh_social_programs', 'unemployment_rate'


# Demographic Characteristics
'black_ratio', 'white_ratio', 'asian_ratio', 'hispanic_ratio', 'hh_w_child_ratio', 'edu_lt_highschool', 'median_age', 'hh_nonfamily_ratio', 'hh_average_size_renter_occupied'

# Built Environment Characteristics
'renter_occ_rate', 'mortgage_status_ratio', 'housing_median_value', '1unit_structure_ratio',
'vacancy_rate'

# Standardize predictors
covariates <- c('poverty_rate', 'hh_median_income', 'median_gross_rent', 'gross_rent_mt40', 
                'hh_social_programs', 'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 'hispanic_ratio', 'edu_lt_highschool',
                'median_age', 'hh_nonfamily_ratio', 'hh_w_child_ratio', 'renter_occ_rate', 'mortgage_status_ratio',
                'housing_median_value', '1unit_structure_ratio', 'vacancy_rate', 'hh_average_size_renter_occupied',
                'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60')
df[covariates] <- scale(df[covariates])
df_np[covariates] <- scale(df_np[covariates])
dim(df)
df$case_number

# Address missing values
## hh_median_income
obs_medinc_idx <- which(!is.na(df$hh_median_income))
miss_medinc_idx <- which(is.na(df$hh_median_income))
medinc_obs = df$hh_median_income[obs_medinc_idx]

## median_gross_rent
obs_medrent_idx <- which(!is.na(df$median_gross_rent))
miss_medrent_idx <- which(is.na(df$median_gross_rent))
medrent_obs = df$median_gross_rent[obs_medrent_idx]

## housing_median_value
obs_medvalue_idx <- which(!is.na(df$housing_median_value))
miss_medvalue_idx <- which(is.na(df$housing_median_value))
medvalue_obs = df$housing_median_value[obs_medvalue_idx]

## hh_average_size_renter_occupied
obs_renter_hhsize_idx <- which(!is.na(df$hh_average_size_renter_occupied))
miss_renter_hhsize_idx <- which(is.na(df$hh_average_size_renter_occupied))
renter_hhsize_obs = df$hh_average_size_renter_occupied[obs_renter_hhsize_idx]


# Set up the data list for Stan
stan_data <- list(N = nrow(df), 
                  N_obs_medinc = length(obs_medinc_idx),
                  N_miss_medinc = length(miss_medinc_idx),
                  N_obs_medrent = length(obs_medrent_idx),
                  N_miss_medrent = length(miss_medrent_idx),
                  N_obs_medvalue = length(obs_medvalue_idx),
                  N_miss_medvalue = length(miss_medvalue_idx),
                  N_obs_renter_hhsize = length(obs_renter_hhsize_idx),
                  N_miss_renter_hhsize = length(miss_renter_hhsize_idx),
                  Y = df$case_number,
                  medinc_obs = medinc_obs,
                  obs_medinc_idx = obs_medinc_idx,
                  miss_medinc_idx = miss_medinc_idx,
                  medrent_obs = medrent_obs,
                  obs_medrent_idx = obs_medrent_idx,
                  miss_medrent_idx = miss_medrent_idx,
                  medvalue_obs = medvalue_obs,
                  obs_medvalue_idx = obs_medvalue_idx,
                  miss_medvalue_idx = miss_medvalue_idx,
                  renter_hhsize_obs = renter_hhsize_obs,
                  obs_renter_hhsize_idx = obs_renter_hhsize_idx,
                  miss_renter_hhsize_idx = miss_renter_hhsize_idx,
                  X_other = as.matrix(df[,c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio',
                                            'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 'hispanic_ratio', 'edu_lt_highschool', 
                                            'median_age', 'hh_nonfamily_ratio', 'renter_occ_rate', 
                                            'mortgage_status_ratio', '1unit_structure_ratio', 'vacancy_rate')]),
                  W = W,
                  num_neighbors=df$total_population,
                  tau = 1)


# Fit the model
fit <- stan(file = 'evictionSTAN4.stan', data = stan_data, 
            iter=2000, chains=4, warmup=1000)

summary(fit)

# Extract the results
posterior_estimates <- rstan::extract(fit)

# Extract the fixed effects coefficients
intercept_samples <- posterior_estimates$alpha
medinc_samples <- posterior_estimates$beta[,1]
medrent_samples <- posterior_estimates$beta[,2]
medvalue_samples <- posterior_estimates$beta[,3]
renter_hhsize_samples <- posterior_estimates$beta[,4]
poverty_samples <- posterior_estimates$beta[,5]
rent_mt40_samples <- posterior_estimates$beta[,6]
social_program_samples <- posterior_estimates$beta[,7]
unemp_samples <- posterior_estimates$beta[,8]
black_ratio_samples <- posterior_estimates$beta[,9]
white_ratio_samples <- posterior_estimates$beta[,10]
edu_lt_hs_samples <- posterior_estimates$beta[,11]
medage_samples <- posterior_estimates$beta[,12]
nonfam_samples <- posterior_estimates$beta[,13]
renter_occ_rate_samples <- posterior_estimates$beta[,14]
mort_ratio_samples <- posterior_estimates$beta[,15]
unit1_structure_samples <- posterior_estimates$beta[,16]
vacancy_rate_samples <- posterior_estimates$beta[,17]

dim(intercept_samples)

# Create a data frame for 4000 samples
df_samples <- data_frame(intercept_samples, medinc_samples, medrent_samples, medvalue_samples, 
                         renter_hhsize_samples, poverty_samples, rent_mt40_samples, social_program_samples, 
                         unemp_samples, black_ratio_samples, white_ratio_samples, edu_lt_hs_samples, 
                         medage_samples, nonfam_samples, renter_occ_rate_samples, mort_ratio_samples, 
                         unit1_structure_samples, vacancy_rate_samples)

df_95ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.025, 0.975))))
df_mean <- data_frame(sapply(df_samples, function(x) mean(x)))
dim(df_95ci)
dim(df_mean)
df_95ci <- cbind(df_95ci, df_mean)
write.csv(df_95ci, 'df_95ci.csv')

# Extract the spatial random effects
spatial_effects <- posterior_estimates$phi
avg_spatial_effects <- apply(spatial_effects, 2, mean)
df$spatial_effect <- avg_spatial_effects
df_geom$spatial_effect <- avg_spatial_effects

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect), color=NA) +
  scale_fill_viridis_c() +
  labs(title="Spatial Random Effects", fill="Effect") +
  theme_minimal()

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect), color=NA) +
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


# Mean estimate and 95% Credible intervals
quantile(medinc_samples, probs=c(0.025, 0.5, 0.975))
mean(medinc_samples)

traceplot(fit, pars = c("alpha"))
summary(fit)

head(df_geom)

df_geom %>% 
  arrange(desc(spatial_effect)) %>% 
  select(GEOID, case_number, spatial_effect, total_population) %>% 
  head(10) %>% 
  ggplot() +
  geom_sf(aes(fill=spatial_effect), color=NA) +
  scale_fill_viridis_c() +
  theme_minimal()
  





#install.packages("maptools", repos="http://R-Forge.R-project.org")
#install.packages("rgeos")

library(maptools)
library(maps)
library(rgeos)
# Moran Test
plot(df_geom['case_number'])
ggplot(df_geom, aes(fill=case_number)) + geom_sf()
Dalls_map <- map('tract', 'dallas', fill=T, plot=T)
moran.test(df_geom$case_number, nb2listw(nb_q, style='B'))
nb_knn1 <- knn2nb(knearneigh(centroids, k=1))




######################################################
# Plotting with base R graphics

par(mfrow=c(2,2))

# Histogram for beta1
hist(black_ratio_samples, main="Black Ratio", xlab="Beta_black_ratio", border="blue", col="lightblue")

# Histogram for beta2
hist(edu_lt_hs_samples, main="Education less than high school", xlab="Beta_edu_lt_hs", border="blue", col="lightblue")

# Histogram for beta3
hist(medinc_samples, main="Household Median Income", xlab="Beta_median_income", border="blue", col="lightblue")

# Histogram for beta4
hist(renter_occ_rate_samples, main="Total Households", xlab="Beta_total_household", border="blue", col="lightblue")

# Trace plots for "black_ratio", "education_lt_hs", "hh_median_income", "total_hh"

# Extract the samples for the beta coefficients
beta_samples <- extract(fit)$beta

# Convert to a data frame for easier plotting with ggplot2
beta_df <- as.data.frame(t(beta_samples))

# Rename the columns to match the predictor names
names(beta_df) <- c("Predictor1", "Predictor2", "Predictor3", "Predictor4")

# Gather the data for plotting using tidyr
library(tidyr)
beta_long <- gather(beta_df, key = "Predictor", value = "Value", factor_key=TRUE)

# Plot using ggplot2
library(ggplot2)
ggplot(beta_long, aes(x = 1:NROW(Value), y = Value, color = Predictor)) +
  geom_line() +
  facet_wrap(~Predictor, scales = 'free_y') +
  theme_minimal() +
  labs(x = "Iteration", y = "Sampled Value", title = "Trace Plots for Predictors") +
  theme(legend.position = "none")


library(maps)
#install.packages("maptools", repos="http://R-Forge.R-project.org")
library(maptools)

df_geom
help(package='maps')
DL_map <- map('county', 'texas', fill=T, plot=T)
map('county', 'texas', fill = TRUE, col = palette())
nb_q <- poly2nb(df_geom, queen=TRUE)
par(mfrow=c(1,1))
centroids <- gCentroid(df_geom, byid=T)
plot(df_geom, border='grey')

library(spdep)
plot(nb_q, coordinates)


####################################
####################################
####################################
# MCMC for df_np
# Standardize predictors
covariates <- c('poverty_rate', 'hh_median_income', 'median_gross_rent', 'gross_rent_mt40', 
                'hh_social_programs', 'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 'hispanic_ratio', 'edu_lt_highschool',
                'median_age', 'hh_nonfamily_ratio', 'hh_w_child_ratio', 'renter_occ_rate', 'mortgage_status_ratio',
                'housing_median_value', '1unit_structure_ratio', 'vacancy_rate', 'hh_average_size_renter_occupied')
df_np[covariates] <- scale(df_np[covariates])
df_np[covariates] <- scale(df_np[covariates])
dim(df_np)
# Address missing values
## hh_median_income
obs_medinc_idx <- which(!is.na(df_np$hh_median_income))
miss_medinc_idx <- which(is.na(df_np$hh_median_income))
medinc_obs = df_np$hh_median_income[obs_medinc_idx]

## median_gross_rent
obs_medrent_idx <- which(!is.na(df_np$median_gross_rent))
miss_medrent_idx <- which(is.na(df_np$median_gross_rent))
medrent_obs = df_np$median_gross_rent[obs_medrent_idx]

## housing_median_value
obs_medvalue_idx <- which(!is.na(df_np$housing_median_value))
miss_medvalue_idx <- which(is.na(df_np$housing_median_value))
medvalue_obs = df_np$housing_median_value[obs_medvalue_idx]

## hh_average_size_renter_occupied
obs_renter_hhsize_idx <- which(!is.na(df_np$hh_average_size_renter_occupied))
miss_renter_hhsize_idx <- which(is.na(df_np$hh_average_size_renter_occupied))
renter_hhsize_obs = df_np$hh_average_size_renter_occupied[obs_renter_hhsize_idx]


# Set up the data list for Stan
stan_data <- list(N = nrow(df_np), 
                  N_obs_medinc = length(obs_medinc_idx),
                  N_miss_medinc = length(miss_medinc_idx),
                  N_obs_medrent = length(obs_medrent_idx),
                  N_miss_medrent = length(miss_medrent_idx),
                  N_obs_medvalue = length(obs_medvalue_idx),
                  N_miss_medvalue = length(miss_medvalue_idx),
                  N_obs_renter_hhsize = length(obs_renter_hhsize_idx),
                  N_miss_renter_hhsize = length(miss_renter_hhsize_idx),
                  Y = df_np$case_number,
                  medinc_obs = medinc_obs,
                  obs_medinc_idx = obs_medinc_idx,
                  miss_medinc_idx = miss_medinc_idx,
                  medrent_obs = medrent_obs,
                  obs_medrent_idx = obs_medrent_idx,
                  miss_medrent_idx = miss_medrent_idx,
                  medvalue_obs = medvalue_obs,
                  obs_medvalue_idx = obs_medvalue_idx,
                  miss_medvalue_idx = miss_medvalue_idx,
                  renter_hhsize_obs = renter_hhsize_obs,
                  obs_renter_hhsize_idx = obs_renter_hhsize_idx,
                  miss_renter_hhsize_idx = miss_renter_hhsize_idx,
                  X_other = as.matrix(df[,c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio',
                                            'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 'hispanic_ratio', 'edu_lt_highschool', 
                                            'median_age', 'hh_nonfamily_ratio', 'renter_occ_rate', 
                                            'mortgage_status_ratio', '1unit_structure_ratio', 'vacancy_rate')]),
                  W = W,
                  num_neighbors=df_np$total_population,
                  tau = 1)


# Fit the model
fit <- stan(file = 'evictionSTAN4.stan', data = stan_data, 
            iter=2000, chains=4, warmup=1000)

summary(fit)
# Extract the results
posterior_estimates <- rstan::extract(fit)


# Extract the fixed effects coefficients
intercept_samples <- posterior_estimates$alpha
medinc_samples <- posterior_estimates$beta[,1]
medrent_samples <- posterior_estimates$beta[,2]
medvalue_samples <- posterior_estimates$beta[,3]
renter_hhsize_samples <- posterior_estimates$beta[,4]
poverty_samples <- posterior_estimates$beta[,5]
rent_mt40_samples <- posterior_estimates$beta[,6]
social_program_samples <- posterior_estimates$beta[,7]
hh_w_child_ratio_samples <- posterior_estimates$beta[,8]
unemp_samples <- posterior_estimates$beta[,9]
black_ratio_samples <- posterior_estimates$beta[,10]
white_ratio_samples <- posterior_estimates$beta[,11]
asian_ratio_samples <- posterior_estimates$beta[,12]
hispanic_ratio_samples <- posterior_estimates$beta[,13]
edu_lt_hs_samples <- posterior_estimates$beta[,14]
medage_samples <- posterior_estimates$beta[,15]
nonfam_samples <- posterior_estimates$beta[,16]
renter_occ_rate_samples <- posterior_estimates$beta[,17]
mort_ratio_samples <- posterior_estimates$beta[,18]
unit1_structure_samples <- posterior_estimates$beta[,19]
vacancy_rate_samples <- posterior_estimates$beta[,20]

dim(intercept_samples)

# Create a data frame for 4000 samples
df_np_samples <- data_frame(intercept_samples, medinc_samples, medrent_samples, medvalue_samples, 
                         renter_hhsize_samples, poverty_samples, rent_mt40_samples, social_program_samples,
                         hh_w_child_ratio_samples, unemp_samples, black_ratio_samples, white_ratio_samples,
                         asian_ratio_samples, hispanic_ratio_samples, edu_lt_hs_samples, 
                         medage_samples, nonfam_samples, renter_occ_rate_samples, mort_ratio_samples, 
                         unit1_structure_samples, vacancy_rate_samples)

df_np_95ci <- t(sapply(df_np_samples, function(x) quantile(x, probs = c(0.025, 0.975))))
df_np_mean <- data_frame(sapply(df_np_samples, function(x) mean(x)))
dim(df_np_95ci)
dim(df_np_mean)
df_np_95ci <- cbind(df_np_95ci, df_np_mean)
write.csv(df_np_95ci, 'df_np_95ci.csv')

# Extract the spatial random effects
spatial_effects <- posterior_estimates$phi
avg_spatial_effects <- apply(spatial_effects, 2, mean)
df$spatial_effect <- avg_spatial_effects
df_geom$spatial_effect <- avg_spatial_effects

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect), color=NA) +
  scale_fill_viridis_c() +
  labs(title="Spatial Random Effects", fill="Effect") +
  theme_minimal()

ggplot(df_geom) +
  geom_sf(aes(fill=spatial_effect), color=NA) +
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


# Mean estimate and 95% Credible intervals
quantile(medinc_samples, probs=c(0.025, 0.5, 0.975))
mean(medinc_samples)

traceplot(fit, pars = c("alpha"))
summary(fit)

head(df_geom)

df_geom %>% 
  arrange(desc(spatial_effect)) %>% 
  select(GEOID, case_number, spatial_effect, total_population) %>% 
  head(10) %>% 
  ggplot() +
  geom_sf(aes(fill=spatial_effect), color=NA) +
  scale_fill_viridis_c() +
  theme_minimal()
