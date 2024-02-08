#install.packages("httpgd")
library(tidyverse)

# Read in data
df_nonpay <- read_csv("data/results/df_95ci_nonpayment_final_6.csv")
head(df_nonpay)
colnames(df_nonpay) <- c("fixed", "lb_95", "ub_95", "beta_hat", "lb_90", "ub_90")

df_other <- read_csv("data/results/df_95ci_other_final_7_small.csv")
colnames(df_other) <- c("fixed", "lb_95", "ub_95", "beta_hat", "lb_90", "ub_90")

unique(df_nonpay$fixed)
length(unique(df_other$fixed))

df_nonpay$fixed <- c("intercept_samples", "rent >= 50%", "public assistance",
"education >= graduate", "children w/ married couple", "children w/ male hher", 
"children w/ female hher", "unemployment", 
"black population", "hispanic population", "asian population", 
"median age", "non-family households", "mortgage ratio",
"single-unit str", "multi-unit str", "vacancy ratio", "median rent increase", 
"house value increase",
"time to work < 30m", "time to work >= 60m", "no internet", "median income",
"median rent", "median house value", "spatial_effects_samples")

df_other$fixed <- c("intercept_samples", "rent >= 50%", "public assistance",
"education >= graduate", "children w/ married couple", "children w/ male hher", 
"children w/ female hher", "unemployment", 
"black population", "hispanic population", "asian population", 
"median age", "non-family households", "mortgage ratio",
"single-unit str", "multi-unit str", "vacancy ratio", "median rent increase", 
"house value increase",
"time to work < 30m", "time to work >= 60m", "no internet", "median income",
"median rent", "median house value", "spatial_effects_samples")

# c("intercept_samples", "rent >= 50%", "social programs",
# "with children", "education >= graduate", "male householder", "female householder", "unemployment", 
# "black pop", "hispanic pop", "median age", "living alone", "mortgage ratio",
# "renter ratio", "single-unit str", "vacancy ratio", "median rent increase", "median value increase",
# "time to work < 30m", "time to work >= 60m", "no internet", "median income",
# "median rent", "median house value", "spatial_effects_samples")

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


economic <- c("rent >= 50%", "public assistance",
                "unemployment", "median income")
demographic <- c("median age", "black population", "hispanic population", "asian population", "education >= graduate",
                "children w/ married couple", "children w/ male hher", "children w/ female hher", 
                "non-family households")
housing <- c("median rent", "renter ratio", "mortgage ratio",
                "vacancy ratio", "median rent increase",
                "median house value", "house value increase")
environmental <- c("time to work < 30m", "time to work >= 60m", "no internet", 
                    "single-unit str", "multi-unit str")
intercept <- c("intercept_samples")
spatial <- c("spatial_effects_samples")

df_nonpay <- df_nonpay %>% 
            mutate(category = case_when(
                fixed %in% economic ~ "Economic Hardship",
                fixed %in% demographic ~ "Demographic Characteristics",
                fixed %in% housing ~ "Housing Market Dynamics",
                fixed %in% environmental ~ "Built Environment",
                fixed %in% intercept ~ "intercept",
                fixed %in% spatial ~ "spatial"
            ))
head(df_nonpay)

df_other <- df_other %>% 
            mutate(category = case_when(
                fixed %in% economic ~ "Economic Hardship",
                fixed %in% demographic ~ "Demographic Characteristics",
                fixed %in% housing ~ "Housing Market Dynamics",
                fixed %in% environmental ~ "Built Environment",
                fixed %in% intercept ~ "intercept",
                fixed %in% spatial ~ "spatial"
            ))

head(df_nonpay)
dim(df_nonpay)
head(df_other)

df <- rbind(df_nonpay, df_other)
dim(df)
df$model <- c(rep("nonpayment", 26), rep("other", 26))
df$category_f <- factor(df$category, levels = c("Economic Hardship", "Demographic Characteristics", "Housing Market Dynamics", "Built Environment", "intercept", "spatial"))
head(df)
View(df)
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
    
######################
######################

Zissou1 <- c("#2ca1db", "#112047", "#f44323", "#dfb78e", "#ccd5dd")


# Economic
pos_econ <- position_nudge(y=c(rep(0.2, 4), rep(-0.2, 4)))
df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples", "multi-unit str")) %>%
    filter(category == "Economic Hardship") %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.15, position=pos_econ) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), size=2, height=0, position=pos_econ) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_point(aes(beta_hat), size=2.5, color='black', shape=2, position=pos_econ) +
    labs(x="", y="") +
    theme_bw() +
    scale_color_manual(values=c(Zissou1[3], Zissou1[1])) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme(plot.title = element_text(hjust = 0.5)) -> econ_plot

# Demographic
pos_demo <- position_nudge(y=c(rep(0.2, 9), rep(-0.2, 9)))
df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    filter(category == "Demographic Characteristics") %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.3, position=pos_demo) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), size=2, height=0, position=pos_demo) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_point(aes(beta_hat), size=2.5, color='black', shape=2, position=pos_demo) +
    labs(x="beta_hat", y="") +
    theme_bw() +
    scale_color_manual(values=c(Zissou1[3], Zissou1[1])) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme(plot.title = element_text(hjust = 0.5)) -> demo_plot

# Housing Market Dynamics (HMD)
pos_hmd <- position_nudge(y=c(rep(0.2, 6), rep(-0.2, 6)))
df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples")) %>%
    filter(category == "Housing Market Dynamics") %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.3, position=pos_hmd) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), size=2, height=0, position=pos_hmd) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_point(aes(beta_hat), size=2.5, color='black', shape=2, position=pos_hmd) +
    labs(x="", y="") +
    theme_bw() +
    scale_color_manual(values=c(Zissou1[3], Zissou1[1])) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme(plot.title = element_text(hjust = 0.5)) -> hmd_plot

# Built Environment (BE)
pos_be <- position_nudge(y=c(rep(0.2, 4), rep(-0.2, 4)))
df %>%
    #filter(!str_detect(fixed, "time")) %>% # Remove time effects
    filter(fixed %ni% c("spatial_effects_samples", "intercept_samples", "multi-unit str")) %>%
    filter(category == "Built Environment") %>%
    ggplot(aes(x=beta_hat, y=reorder(fixed, beta_hat), color=model)) +
    geom_errorbarh(aes(xmin=lb_95, xmax=ub_95), height=0.15, position=pos_be) +
    geom_errorbarh(aes(xmin=lb_90, xmax=ub_90), size=2, height=0, position=pos_be) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_point(aes(beta_hat), size=2.5, color='black', shape=2, position=pos_be) +
    labs(x="beta_hat", y="") +
    theme_bw() +
    scale_color_manual(values=c(Zissou1[3], Zissou1[1])) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    theme(plot.title = element_text(hjust = 0.5)) -> be_plot

library(ggpubr)
fig_results <- ggarrange(econ_plot, hmd_plot, demo_plot, be_plot, 
            ncol=2, nrow=2, 
            align="hv",
            # labels=c("Economic Factors", "Housing Market Dynamics", "Demographic Factors", "Built Environment"),
            # label.y = 1.1,
            common.legend = TRUE, legend="bottom")
View(df)
?ggexport
ggexport(fig_results, filename = "results_visualization6.pdf", width = 10, height = 7, units = "in", dpi = 300)
