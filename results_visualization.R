#install.packages("httpgd")
library(tidyverse)

# Read in data
df_nonpay <- read_csv("data/results/df_95ci_final_nonpayment.csv")
df_nonpay <- df_nonpay[,c(1,5:9)]
colnames(df_nonpay) <- c("fixed", "lb_95", "ub_95", "beta_hat", "lb_90", "ub_90")

df_other <- read_csv("data/results/df_95ci_final_other.csv")
df_other <- df_other[,c(1,5:7)]
colnames(df_other) <- c("fixed", "lb_95", "ub_95", "beta_hat")

df_nonpay$fixed <- c("intercept_samples", "poverty", "rent >= 50%", "social programs",
"with child", "unemployment", "black pop", "white pop",
"asian pop", "hispanic pop", "education <= highschool", "median age",
"nonfamily", "renter ratio", "mortgage ratio", "single-unit str",
"vacancy ratio", "median rent increase", "time to work < 30m",
"time to work 30 to 59m", "time to work >= 60m", "median income",
"median rent", "median house value", "renter hhsize", "spatial_effects_samples")

df_other$fixed <- c("intercept_samples", "poverty", "rent >= 50%", "social programs",
"with child", "unemployment", "black pop", "white pop",
"asian pop", "hispanic pop", "education <= highschool", "median age",
"nonfamily", "renter ratio", "mortgage ratio", "single-unit str",
"vacancy ratio", "median rent increase", "time to work < 30m",
"time to work 30 to 59m", "time to work >= 60m", "median income",
"median rent", "median house value", "renter hhsize", "spatial_effects_samples")

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

economic <- c("poverty", "rent >= 50%", "social programs",
                "unemployment", "median rent", "median income")
demographic <- c("median age", "black pop", "white pop", 
                "asian pop", "hispanic pop", "education <= highschool", 
                "nonfamily", "with child", 'renter hhsize')
housing <- c("median rent", "renter ratio", "mortgage ratio",
                "single-unit str", "vacancy ratio", "median rent increase",
                "median house value")
environmental <- c("time to work < 30m", "time to work 30 to 59m", "time to work >= 60m")
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
df_other$lb_90 <- NA
df_other$ub_90 <- NA
df <- rbind(df_nonpay, df_other)
df$model <- c(rep("nonpayment", 26), rep("other", 26))
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

pos <- position_nudge(y = ifelse(df$model == "nonpayment", 0.15, -0.15))

df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.2, position=pos) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), color='red', size=2, height=0, position=pos) +
    geom_point(aes(beta_hat), size=3, color='black', shape=2, position=pos) +
    geom_vline(xintercept=0, linetype="dashed") +
    labs(x="$\hat{\beta}$", y="Fixed Effect") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~category_f, scales= "free")
    
View(df)
    