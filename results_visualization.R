#install.packages("httpgd")
library(tidyverse)

# Read in data
df_nonpay <- read_csv("data/results/df_95ci_nonpayment_final_2.csv")
head(df_nonpay)
colnames(df_nonpay) <- c("fixed", "lb_95", "ub_95", "beta_hat", "lb_90", "ub_90")

df_other <- read_csv("data/results/df_95ci_other_final_2.csv")
colnames(df_other) <- c("fixed", "lb_95", "ub_95", "beta_hat", "lb_90", "ub_90")

c('gross_rent_mt50', 'hh_social_programs', 'hh_w_child_ratio', 'edu_grad',
                'hh_w_child_male_hh_ratio', 'hh_w_child_female_hh_ratio',
                'unemployment_rate', 'black_ratio', 'hispanic_ratio', 
                'median_age', 'hher_living_alone_ratio', 'mortgage_status_ratio',
                'renter_occ_rate', 'oneunit_structure_ratio', 'vacancy_rate', 
                'median_gross_rent_change', 'housing_median_value_change',
                'time_to_work_lt30', 'time_to_work_mt60', 'no_internet_access_ratio', 
                'hh_median_income', 'median_gross_rent', 'housing_median_value')

df_nonpay$fixed <- c("intercept_samples", "rent >= 50%", "social programs",
"with children", "education >= graduate", "male householder", "female householder", "unemployment", 
"black pop", "hispanic pop", "median age", "living alone", "mortgage ratio",
"renter ratio", "single-unit str", "vacancy ratio", "median rent increase", "median value increase",
"time to work < 30m", "time to work >= 60m", "no internet", "median income",
"median rent", "median house value", "spatial_effects_samples")

df_other$fixed <- c("intercept_samples", "rent >= 50%", "social programs",
"with children", "education >= graduate", "male householder", "female householder", "unemployment", 
"black pop", "hispanic pop", "median age", "living alone", "mortgage ratio",
"renter ratio", "single-unit str", "vacancy ratio", "median rent increase", "median value increase",
"time to work < 30m", "time to work >= 60m", "no internet", "median income",
"median rent", "median house value", "spatial_effects_samples")

`%ni%` <- Negate(`%in%`)
df_nonpay %>%
    filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    ggplot(aes(x=beta_hat, y=fixed)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.2) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), color='red', size=3, height=0) +
    geom_point(aes(beta_hat), size=5, shape=17) +
    geom_vline(xintercept=0, linetype="dashed") +
    labs(x="Beta", y="Fixed Effect", title="Nonpayment Fixed Effects") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

c("intercept_samples", "rent >= 50%", "social programs",
"with children", "education >= graduate", "male householder", "female householder", "unemployment", 
"black pop", "hispanic pop", "median age", "living alone", "mortgage ratio",
"renter ratio", "single-unit str", "vacancy ratio", "median rent increase", "median value increase",
"time to work < 30m", "time to work >= 60m", "no internet", "median income",
"median rent", "median house value", "spatial_effects_samples")

economic <- c("rent >= 50%", "social programs",
                "unemployment", "median income")
demographic <- c("median age", "black pop", "hispanic pop", "education >= graduate", 
                "living alone", "with children", 'male householder', 'female householder')
housing <- c("median rent", "renter ratio", "mortgage ratio",
                "single-unit str", "vacancy ratio", "median rent increase",
                "median house value", "median value increase")
environmental <- c("time to work < 30m", "time to work >= 60m", "no internet")
intercept <- c("intercept_samples")
spatial <- c("spatial_effects_samples")

df_nonpay <- df_nonpay %>% 
            mutate(category = case_when(
                fixed %in% economic ~ "Economic",
                fixed %in% demographic ~ "Demographic",
                fixed %in% housing ~ "Housing Market Dynamics",
                fixed %in% environmental ~ "Built Environment",
                fixed %in% intercept ~ "intercept",
                fixed %in% spatial ~ "spatial"
            ))
head(df_nonpay)

df_other <- df_other %>% 
            mutate(category = case_when(
                fixed %in% economic ~ "Economic",
                fixed %in% demographic ~ "Demographic",
                fixed %in% housing ~ "Housing Market Dynamics",
                fixed %in% environmental ~ "Built Environment",
                fixed %in% intercept ~ "intercept",
                fixed %in% spatial ~ "spatial"
            ))

head(df_nonpay)
dim(df_nonpay)
head(df_other)

df <- rbind(df_nonpay, df_other)
df$model <- c(rep("nonpayment", 25), rep("other", 25))
df$category_f <- factor(df$category, levels = c("Economic", "Demographic", "Housing Market Dynamics", "Built Environment", "intercept", "spatial"))
head(df)
# Visualize results
df_nonpay %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat))) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.2) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), color='red', size=2, height=0) +
    geom_point(aes(beta_hat), size=3.5, shape=17) +
    geom_vline(xintercept=0, linetype="dashed") +
    labs(x="Beta", y="Fixed Effect", title="Nonpayment Fixed Effects") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~category, scales= "free")

pos <- position_nudge(y = ifelse(df$model == "nonpayment", 0.2, -0.2))

df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model), position=pos) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.2, position=pos) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), size=2, height=0, position=pos) +
    geom_point(aes(beta_hat), size=3, color='black', shape=2, position=pos) +
    geom_vline(xintercept=0, linetype="dashed") +
    labs(x="beta_hat", y="Fixed Effect") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~category_f, scales= "free_y")
    
View(df)
    