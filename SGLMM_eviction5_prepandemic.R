library(tidyverse)
library(rstan)
library(sf)
library(spdep)
library(mice)

#rm(list=ls())
# Load the main data
df <- read_csv("data/acs_evic_data_wo_zero_pop_prepandemic.csv")
#df_np <- read_csv("data/acs_evic_data_np_wo_zero_pop.csv")

df_geom <- df
#df_np_geom <- df_np
# Convert df_geom to SF object using geometry information
df_geom = st_as_sf(df_geom, wkt = "geometry_x")

#df_np_geom <- st_as_sf(df_np_geom, wkt = "geometry_x")
head(df_geom$geometry_x)
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
covariates <- c('gross_rent_mt50', 'hh_social_programs', 'edu_grad', 'children_w_married_couple_ratio',
                'children_w_male_hh_ratio', 'children_w_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 'asian_ratio',
                'median_age', 'hh_nonfamily_ratio', 'mortgage_status_ratio',
                '1unit_structure_ratio', 'multiunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

# covariates <- c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
#                 'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
#                 'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
#                 'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
#                 'renter_occ_rate', '1unit_structure_ratio', 'vacancy_rate', 
#                 'median_gross_rent_change', 'housing_median_value_change',
#                 'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
#                 'hh_median_income', 'median_gross_rent', 'housing_median_value')


df2 <- df[covariates]
# colnames(df2) <- c('poverty_rate', 'gross_rent_mt40', 'hh_social_programs', 'hh_w_child_ratio', 
#                    'unemployment_rate', 'black_ratio', 'white_ratio', 'asian_ratio', 
#                    'hispanic_ratio', 'edu_lt_highschool','median_age', 'hh_nonfamily_ratio', 
#                    'renter_occ_rate', 'mortgage_status_ratio', 'oneunit_structure_ratio', 'vacancy_rate', 
#                    'median_gross_rent_change', 'time_to_work_lt30', 'time_to_work_30to59', 'time_to_work_mt60',
#                    'hh_median_income', 'median_gross_rent',  'housing_median_value', 'hh_average_size_renter_occupied')
colnames(df2) <- c('gross_rent_mt50', 'hh_social_programs', 'edu_grad', 'children_w_married_couple_ratio',
                'children_w_male_hh_ratio', 'children_w_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 'asian_ratio',
                'median_age', 'hh_nonfamily_ratio', 'mortgage_status_ratio',
                'oneunit_structure_ratio', 'multiunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')


summary(df2)

# Check missing values pattern
md.pattern(df2)

# Impute missing values using MICE
imputed <- mice(df2, m=5, maxit=100, method='pmm', seed=123)
df3 <- complete(imputed, 2)
summary(df3)

# Standardize predictors

covariates <- c('gross_rent_mt50', 'hh_social_programs', 'edu_grad', 'children_w_married_couple_ratio',
                'children_w_male_hh_ratio', 'children_w_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 'asian_ratio',
                'median_age', 'hh_nonfamily_ratio', 'mortgage_status_ratio',
                'oneunit_structure_ratio', 'multiunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')


df3[covariates] <- scale(df3[covariates])
df3 <- cbind(df$case_number, df$eviction_rate_renter, df3)
colnames(df3)[1:2] <- c("case_number", "eviction_rate_renter")
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
                  Y = df3$eviction_rate_renter,
                  X = X,
                  A_sparse = A_sparse,
                  P_perp = P_perp,
                  XX_inv_tx = XX_inv_tx,
                  num_neighbors=colSums(A),
                  tau = 10)

# Fit the model
fit_nonpayment <- stan(file = 'sglmm_orthog_eff_small.stan', data = stan_data, 
            iter=18000, chains=4, cores=4, warmup=3000, thin=3,
            control = list(adapt_delta = 0.9, max_treedepth = 15))

summary(fit_nonpayment, probs=c(0.5))$summary

getwd()
check_hmc_diagnostics(fit_nonpayment)
# Save the fitted model
fit_nonpayment@stanmodel@dso <- new('cxxdso')
saveRDS(fit_nonpayment, file='data/results/fit_nonpayment_prepandemic.rds')

# Load the fitted model
fit_nonpayment <- readRDS("data/results/fit_nonpayment_final6.rds")

# Extract the results
posterior_estimates <- rstan::extract(fit_nonpayment)

stan_trace(fit_nonpayment, pars=c("beta_orig"))
stan_trace(fit_nonpayment, pars=c("beta"))
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

c('gross_rent_mt50', 'hh_social_programs', 'edu_grad', 'children_w_married_couple_ratio',
                'children_w_male_hh_ratio', 'children_w_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 'asian_ratio',
                'median_age', 'hh_nonfamily_ratio', 'mortgage_status_ratio',
                'oneunit_structure_ratio', 'multiunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

intercept_samples <- posterior_estimates$alpha
rent_mt50_samples <- posterior_estimates$beta_orig[,1]
social_program_samples <- posterior_estimates$beta_orig[,2]
edu_grad_samples <- posterior_estimates$beta_orig[,3]
children_w_married_couple_samples <- posterior_estimates$beta_orig[,4]
children_w_male_samples <- posterior_estimates$beta_orig[,5]
children_w_female_samples <- posterior_estimates$beta_orig[,6]
unemp_samples <- posterior_estimates$beta_orig[,7]
black_ratio_samples <- posterior_estimates$beta_orig[,8]
hispanic_ratio_samples <- posterior_estimates$beta_orig[,9]
asian_ratio_samples <- posterior_estimates$beta_orig[,10]
medage_samples <- posterior_estimates$beta_orig[,11]
nonfamily_samples <- posterior_estimates$beta_orig[,12]
mort_ratio_samples <- posterior_estimates$beta_orig[,13]
unit1_structure_samples <- posterior_estimates$beta_orig[,14]
multiunit_structure_samples <- posterior_estimates$beta_orig[,15]
vacancy_rate_samples <- posterior_estimates$beta_orig[,16]
medrent_change_samples <- posterior_estimates$beta_orig[,17]
medvalue_change_samples <- posterior_estimates$beta_orig[,18]
time_to_work_lt30_samples <- posterior_estimates$beta_orig[,19]
time_to_work_mt60_samples <- posterior_estimates$beta_orig[,20]
no_internet_access_samples <- posterior_estimates$beta_orig[,21]
medinc_samples <- posterior_estimates$beta_orig[,22]
medrent_samples <- posterior_estimates$beta_orig[,23]
medvalue_samples <- posterior_estimates$beta_orig[,24]
spatial_effects_samples <- posterior_estimates$W_transformed

dim(posterior_estimates$beta_orig)
dim(posterior_estimates$beta)
dim(intercept_samples)

# Create a data frame for 4000 samples
df_samples <- data_frame(intercept_samples, rent_mt50_samples, social_program_samples,
                         edu_grad_samples, children_w_married_couple_samples, children_w_male_samples, children_w_female_samples, 
                         unemp_samples, black_ratio_samples, hispanic_ratio_samples, asian_ratio_samples,
                         medage_samples, nonfamily_samples, mort_ratio_samples, 
                         unit1_structure_samples, multiunit_structure_samples, vacancy_rate_samples, medrent_change_samples,
                         medvalue_change_samples,
                         time_to_work_lt30_samples, time_to_work_mt60_samples, no_internet_access_samples,
                         medinc_samples, medrent_samples, medvalue_samples, spatial_effects_samples)

df_95ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.025, 0.975))))
df_mean <- data_frame(sapply(df_samples, function(x) mean(x)))
dim(df_95ci)
dim(df_mean)
df_95ci <- cbind(df_95ci, df_mean)
View(df_95ci)


# 90% CI
df_90ci <- t(sapply(df_samples, function(x) quantile(x, probs = c(0.05, 0.95))))
df_95ci <- cbind(df_95ci, df_90ci)
write.csv(df_95ci, "data/results/df_95ci_nonpayment_prepandemic.csv")
View(df_95ci)
################################

# Extract the spatial random effects
spatial_effects <- posterior_estimates$W_transformed
dim(spatial_effects)
avg_spatial_effects <- apply(spatial_effects, 2, mean)

# I've already revised this part in slgmm_analysis_prepandemic.ipynb
# # Load removed CBGs
# df_removed <- read_csv('data/eviction_count_bg_2021_for_removed_cbg.csv')
# head(df_removed)

# Add spatial effects to df_np
df$spatial_effect <- avg_spatial_effects

# Merge df_np and df_removed
df <- merge(df, df_removed, by=c('GEOID', 'geometry_x'), all.y=TRUE)
dim(df)
# Convert df_np to SF object
?st_as_sf
class(df$geometry_x)
df <- st_as_sf(df, wkt = "geometry_x", crs=4326)
st_crs(df) <- 4326

write.csv(df, 'data/results/df_geom_nonpayment_prepandemic.csv')





plot1 <- ggplot(df) + 
         geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) + 
         scale_fill_viridis_c() + 
         labs(title="Spatial Random Effects", fill="Effect") + 
         theme_minimal()

criteria <- quantile(df$spatial_effect, probs=c(0.025, 0.975), na.rm=TRUE)
Zissou1 <- c('#2ca1db', '#112047', '#f44323', '#dfb78e', '#ccd5dd')
library(ggpubr)

df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria[2], 1, ifelse(spatial_effect <= criteria[1], -1, 0))) %>%
ggplot() + 
         geom_sf(aes(fill=bus_prac, geometry=geometry_x), color=NA) + 
         scale_fill_viridis_c() + 
         labs(title="Spatial Random Effects", fill="Effect") + 
         theme_minimal()

# Load Dallas top filers data
df_top_nonpayment <- read_csv('data/df_nonpay_100_filer.csv')
head(df_top_nonpayment)

fig_nonpayment_se <- df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria[2], 1, ifelse(spatial_effect <= criteria[1], -1, 0))) %>%
mutate(bus_prac = ifelse(is.na(bus_prac), 0, bus_prac)) %>%
ggplot() + 
         geom_sf(aes(fill=factor(bus_prac, label=c('Low', 'Moderate', 'High')), geometry=geometry_x), color='darkgrey') + 
         scale_fill_manual(values=c(Zissou1[1], Zissou1[5], Zissou1[3])) +
         geom_point(data=df_top_nonpayment, aes(x=X, y=Y, size=case_number), color="#00CC99", alpha=.7) +
         scale_size(range=c(.3,10)) +
         labs(fill="Spatial Effects", x="", y="", size="Top 100 Landlords\nFiling Counts\n(2017-2021)") + 
         theme_bw()

ggexport(fig_nonpayment_se, filename = "fig_nonpayment_se.pdf", width = 8, height = 6, units = "in", dpi = 300)

unique(df$bus_prac)


# Diagnostics after fitting
# Visualization with bayesplot package
library(bayesplot)
available_mcmc(pattern='_nuts_')
log_posterior(fit)
mcmc_parcoord(fit, pars=c("beta_orig[1]", "beta_orig[2]", "beta_orig[3]", "beta_orig[4]",
                          "beta_orig[5]", "beta_orig[6]", "beta_orig[7]", "beta_orig[8]",
                          "beta_orig[9]", "beta_orig[10]", "beta_orig[11]", "beta_orig[12]",
                          "beta_orig[13]", "beta_orig[14]", "beta_orig[15]", "beta_orig[16]",
                          "beta_orig[17]", "beta_orig[18]", "beta_orig[19]", "beta_orig[20]",
                          "beta_orig[21]", "beta_orig[22]", "beta_orig[23]"), np=1000)
?mcmc_parcoord
# R-hat
rhats <- rhat(fit)
print(rhats)
rhats[3]
mcmc_rhat(fit$beta[1:10, 1:10])

# Effective sample size
ratios_cp <- neff_ratio(fit)
print(ratios_cp)

# Autocorrelation
mcmc_acf(fit, pars=c("beta_orig[1]", "beta_orig[2]"))
mcmc_acf(fit, pars=c("beta_orig[1]"))

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

# Compare eviction filing rate and spatial effects
speff_criteria <- quantile(df$spatial_effect, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE)
evicfile_criteria <- quantile(df$eviction_rate_renter, probs=c(0.2, 0.4, 0.6, 0.8), na.rm=TRUE)

df %>%
  mutate(speff_cat = ifelse(spatial_effect <= speff_criteria[1], 1, 
                            ifelse(spatial_effect <= speff_criteria[2], 2, 
                                   ifelse(spatial_effect <= speff_criteria[3], 3, 
                                          ifelse(spatial_effect <= speff_criteria[4], 4, 5))))) %>%
  ggplot() +
         geom_sf(aes(fill=speff_cat, geometry=geometry_x), color='white') + 
         scale_fill_viridis_c(option="E") +
         labs(title="Spatial Random Effects", fill="Effect") + 
         theme_minimal() -> plot_spatial

df %>%
  mutate(evicfile_cat = ifelse(eviction_rate_renter <= evicfile_criteria[1], 1, 
                            ifelse(eviction_rate_renter <= evicfile_criteria[2], 2, 
                                   ifelse(eviction_rate_renter <= evicfile_criteria[3], 3, 
                                          ifelse(eviction_rate_renter <= evicfile_criteria[4], 4, 5))))) %>%
  ggplot() +
         geom_sf(aes(fill=evicfile_cat, geometry=geometry_x), color='white') + 
         scale_fill_viridis_c(option="E") + 
         labs(title="Eviction Filing Rate", fill="Effect") + 
         theme_minimal() -> plot_real

ggexport(plot_spatial, filename = "fig_spatial_effect2.pdf", width = 8, height = 6, units = "in", dpi = 300)
ggexport(plot_real, filename = "fig_real_rate2.pdf", width = 8, height = 6, units = "in", dpi = 300)
####################

plot_spatial <- ggplot(df) + 
         geom_sf(aes(fill=spatial_effect, geometry=geometry_x), color=NA) + 
         scale_fill_viridis_c() + 
         labs(title="Spatial Random Effects", fill="Effect") + 
         theme_minimal()


plot_real <- ggplot(df) + 
         geom_sf(aes(fill=log(eviction_rate_renter), geometry=geometry_x), color=NA) + 
         scale_fill_viridis_c() + 
         labs(title="Eviction Filing Rate", fill="Effect") + 
         theme_minimal()

criteria <- quantile(df$spatial_effect, probs=c(0.025, 0.975), na.rm=TRUE)
Zissou1 <- c('#2ca1db', '#112047', '#f44323', '#dfb78e', '#ccd5dd')

df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria[2], 1, ifelse(spatial_effect <= criteria[1], -1, 0))) %>%
ggplot() + 
         geom_sf(aes(fill=bus_prac, geometry=geometry_x), color=NA) + 
         scale_fill_viridis_c() + 
         labs(title="Spatial Random Effects", fill="Effect") + 
         theme_minimal()

# Load Dallas top filers data
df_top_nonpayment <- read_csv('data/df_nonpay_100_geoid.csv')
head(df_top_nonpayment)
View(df_top_nonpayment)
fig_nonpayment_se <- df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria[2], 1, ifelse(spatial_effect <= criteria[1], -1, 0))) %>%
mutate(bus_prac = ifelse(is.na(bus_prac), 0, bus_prac)) %>%
ggplot() + 
         geom_sf(aes(fill=factor(bus_prac, label=c('Low', 'Moderate', 'High')), geometry=geometry_x), color='darkgrey') + 
         scale_fill_manual(values=c(Zissou1[1], Zissou1[5], Zissou1[3])) +
         geom_point(data=df_top_nonpayment, aes(x=X, y=Y, size=case_number), color="#00CC99", alpha=.7) +
         scale_size(range=c(.3,10)) +
         labs(fill="Spatial Effects", x="", y="", size="Top 100 Landlords\nFiling Counts\n(2017-2021)") + 
         theme_bw()

ggexport(fig_nonpayment_se, filename = "fig_nonpayment_se.pdf", width = 8, height = 6, units = "in", dpi = 300)


head(df)
dim(df)
# Figure 3
#criteria <- quantile(df$spatial_effect, probs=c(.68), na.rm=TRUE)
mean_se <- mean(df$spatial_effect, na.rm=TRUE) 
std_se <- sd(df$spatial_effect, na.rm=TRUE)
criteria <- mean_se + std_se

# Create a new temporary dataframe including geoid info for top 100 filers
df_temp <- df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria, 1, 0)) %>%
filter(bus_prac == 1)

df_top_nonpayment <- df_top_nonpayment %>%
mutate(high_se = ifelse(GEOID %in% df_temp$GEOID, 1, 0))


df <- df %>%
mutate(bus_prac = ifelse(spatial_effect >= criteria, 1, 0)) %>%
mutate(bus_prac = ifelse(is.na(bus_prac), 0, bus_prac))
table(df$bus_prac)

# fig_nonpayment_se <- df %>%
#        mutate(bus_prac2 = ifelse(spatial_effect >= criteria, 1, 0)) %>%
#        mutate(bus_prac2 = ifelse(is.na(bus_prac2), 0, bus_prac2)) %>%
#        ggplot() + 
#          geom_sf(aes(fill=factor(bus_prac2, label=c('Moderate', 'High')), geometry=geometry_x), color='darkgrey') + 
#          scale_fill_manual(values=c(Zissou1[5], "#F0DE68")) +
#          geom_point(data=df_top_nonpayment, aes(x=X, y=Y, size=case_number), color="#00CC99", alpha=.5) +
#          scale_size(range=c(.3,10)) +
#          labs(fill="Spatial Effects", x="", y="", size="Top 100 Landlords\nFiling Counts\n(2017-2021)") + 
#          theme_bw()

# ggexport(fig_nonpayment_se, filename = "fig_nonpayment_se_1.pdf", width = 8, height = 6, units = "in", dpi = 300)



fig_nonpayment_se <- df %>%
       mutate(bus_prac2 = ifelse(spatial_effect >= criteria, 1, 0)) %>%
       mutate(bus_prac2 = ifelse(is.na(bus_prac2), 0, bus_prac2)) %>%
       ggplot() + 
         geom_sf(aes(fill=factor(bus_prac2, label=c('Moderate', 'High')), geometry=geometry_x), color='darkgrey') + 
         scale_fill_manual(values=c(Zissou1[5], "#F0DE68")) +
         geom_point(data=df_top_nonpayment, aes(x=X, y=Y, color=factor(high_se)), alpha=.8, size=1.2) +
         scale_color_manual(values=c(Zissou1[1], Zissou1[3])) +
         labs(fill="Spatial Effects", x="", y="") + 
         theme_minimal() +
         theme(legend.position="bottom", panel.grid.major=element_blank(), panel.grid.minor=element_blank())

ggexport(fig_nonpayment_se, filename = "fig_nonpayment_se_2.pdf", width = 6, height = 6, units = "in", dpi = 300)

View(df)
