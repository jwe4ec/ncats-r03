# ---------------------------------------------------------------------------- #
# Run Analyses
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages, set seed ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

groundhog.library(psych, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define functions used in script ----
# ---------------------------------------------------------------------------- #

# Define function to compute Pearson product-moment correlation between one
# variable ("x_var") and one or more others ("y_vars") from wide data frame

cor_test_one_to_many <- function(dat, x_var, y_vars, metric) {
  r     <- vector("double", length(y_vars))
  t     <- vector("double", length(y_vars))
  df    <- vector("double", length(y_vars))
  p     <- vector("double", length(y_vars))
  ci_ll <- vector("double", length(y_vars))
  ci_ul <- vector("double", length(y_vars))
  
  for (i in 1:length(y_vars)) {
    cor_test <- cor.test(dat[, x_var],
                         dat[, y_vars[i]],
                         type = "pearson")
    
    r[i]     <- round(cor_test$estimate, 2)
    t[i]     <- round(cor_test$statistic, 2)
    df[i]    <- cor_test$parameter
    p[i]     <- round(cor_test$p.value, 3)
    ci_ll[i] <- round(cor_test$conf.int[1], 2)
    ci_ul[i] <- round(cor_test$conf.int[2], 2)
  }
  
  res <- data.frame(metric, x_var, y_var = y_vars, 
                    r, t, df, p, ci_ll, ci_ul)
  
  return(res)
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/mdib/scored/mdib_hd_dat.RData")
load("./data/mdib/scored/mdib_pd_dat.RData")
load("./data/mdib/helper/mdib_dat_items.RData")

# ---------------------------------------------------------------------------- #
# Create time point variable, restrict columns, convert to wide format ----
# ---------------------------------------------------------------------------- #

# Create time point variable

mdib_hd_dat$wave <- NA
mdib_hd_dat$wave[mdib_hd_dat$redcap_event_name == "baseline_arm_1"] <- "baseline"
mdib_hd_dat$wave[mdib_hd_dat$redcap_event_name == "followup_arm_1"] <- "followup"

mdib_pd_dat$wave <- NA
mdib_pd_dat$wave[mdib_pd_dat$redcap_event_name == "baseline_arm_1"] <- "baseline"
mdib_pd_dat$wave[mdib_pd_dat$redcap_event_name == "followup_arm_1"] <- "followup"

# Restrict to columns of interest

anlys_cols <- c("mdib_neg_int_m", "mdib_neg_ext_m","bbsiq_neg_int_m", "bbsiq_neg_ext_m", 
                "asi_m", "bfne2_8_m", "neuroqol_anx_m", "sads_red_m")

target_cols <- c("record_id", "wave", anlys_cols)

mdib_hd_dat_short <- mdib_hd_dat[, target_cols]
mdib_pd_dat_short <- mdib_pd_dat[, target_cols]

# Convert to wide format

mdib_hd_dat_short_wide <- reshape(mdib_hd_dat_short, direction = "wide",
                                  idvar = "record_id", timevar = "wave",
                                  v.names = anlys_cols)
mdib_pd_dat_short_wide <- reshape(mdib_pd_dat_short, direction = "wide",
                                  idvar = "record_id", timevar = "wave",
                                  v.names = anlys_cols)

# Remove columns not assessed at certain time points

rm_cols <- c("sads_red_m.baseline", "asi_m.followup", "bfne2_8_m.followup")

mdib_hd_dat_short_wide <- mdib_hd_dat_short_wide[, !(names(mdib_hd_dat_short_wide) %in% rm_cols)]
mdib_pd_dat_short_wide <- mdib_pd_dat_short_wide[, !(names(mdib_pd_dat_short_wide) %in% rm_cols)]

# ---------------------------------------------------------------------------- #
# Analyze test-retest reliability ----
# ---------------------------------------------------------------------------- #

# Compute 2-month test-retest reliability correlation using average item scores
# and pairwise deletion

metric <- "test_retest"

x_var  <- "mdib_neg_int_m.baseline"
y_vars <- "mdib_neg_int_m.followup"

hd_mdib_neg_int_m_retest <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_int_m_retest <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_ext_m.baseline"
y_vars <- "mdib_neg_ext_m.followup"

hd_mdib_neg_ext_m_retest <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_ext_m_retest <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze convergent validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations at baseline using average item scores and pairwise deletion

metric <- "convergent"

x_var  <- "mdib_neg_int_m.baseline"
y_vars <- c("bbsiq_neg_int_m.baseline", "asi_m.baseline", "bfne2_8_m.baseline")

hd_mdib_neg_int_m_conv <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_int_m_conv <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_ext_m.baseline"
y_vars <- c("bbsiq_neg_ext_m.baseline", "asi_m.baseline", "bfne2_8_m.baseline")

hd_mdib_neg_ext_m_conv <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_ext_m_conv <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze concurrent validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations at baseline using average item scores and pairwise deletion

metric <- "concurrent"
y_vars <- "neuroqol_anx_m.baseline"

x_var  <- "mdib_neg_int_m.baseline"

hd_mdib_neg_int_m_conc <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_int_m_conc <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_ext_m.baseline"

hd_mdib_neg_ext_m_conc <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_ext_m_conc <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze predictive validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations from baseline to follow-up using average item scores
# and pairwise deletion

metric <- "predictive"
y_vars <- c("neuroqol_anx_m.followup", "sads_red_m.followup")

x_var  <- "mdib_neg_int_m.baseline"

hd_mdib_neg_int_m_pred <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_int_m_pred <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_ext_m.baseline"

hd_mdib_neg_ext_m_pred <- cor_test_one_to_many(mdib_hd_dat_short_wide, x_var, y_vars, metric)
pd_mdib_neg_ext_m_pred <- cor_test_one_to_many(mdib_pd_dat_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Restrict to baseline ----
# ---------------------------------------------------------------------------- #

mdib_hd_dat_bl <- mdib_hd_dat[mdib_hd_dat$wave == "baseline", ]
mdib_pd_dat_bl <- mdib_pd_dat[mdib_pd_dat$wave == "baseline", ]

# ---------------------------------------------------------------------------- #
# Compute internal consistency ----
# ---------------------------------------------------------------------------- #

# Compute unstandardized Cronbach's alpha given that items are not standardized 
# prior to computing scale scores (see Falk & Savalei, 2011). Compute both point 
# estimates and CIs (see Iacobucci & Duhachek, 2003).

# Compute bootstrapped CIs for negative MDIB scales at baseline using pairwise 
# deletion (default)

hd_mdib_neg_int_bl_alpha <- psych::alpha(mdib_hd_dat_bl[, mdib_dat_items$mdib_neg_int], n.iter = 1000)$boot.ci
pd_mdib_neg_int_bl_alpha <- psych::alpha(mdib_pd_dat_bl[, mdib_dat_items$mdib_neg_int], n.iter = 1000)$boot.ci

hd_mdib_neg_ext_bl_alpha <- psych::alpha(mdib_hd_dat_bl[, mdib_dat_items$mdib_neg_ext], n.iter = 1000)$boot.ci
pd_mdib_neg_ext_bl_alpha <- psych::alpha(mdib_pd_dat_bl[, mdib_dat_items$mdib_neg_ext], n.iter = 1000)$boot.ci

# ---------------------------------------------------------------------------- #
# Save results ----
# ---------------------------------------------------------------------------- #

mdib_hd_res <- list(mdib_neg_int_bl_alpha = hd_mdib_neg_int_bl_alpha, 
                    mdib_neg_ext_bl_alpha = hd_mdib_neg_ext_bl_alpha, 
                    mdib_neg_int_m_retest = hd_mdib_neg_int_m_retest, 
                    mdib_neg_ext_m_retest = hd_mdib_neg_ext_m_retest, 
                    mdib_neg_int_m_conv   = hd_mdib_neg_int_m_conv, 
                    mdib_neg_ext_m_conv   = hd_mdib_neg_ext_m_conv, 
                    mdib_neg_int_m_conc   = hd_mdib_neg_int_m_conc,
                    mdib_neg_ext_m_conc   = hd_mdib_neg_ext_m_conc,
                    mdib_neg_int_m_pred   = hd_mdib_neg_int_m_pred, 
                    mdib_neg_ext_m_pred   = hd_mdib_neg_ext_m_pred)

mdib_pd_res <- list(mdib_neg_int_bl_alpha = pd_mdib_neg_int_bl_alpha, 
                    mdib_neg_ext_bl_alpha = pd_mdib_neg_ext_bl_alpha, 
                    mdib_neg_int_m_retest = pd_mdib_neg_int_m_retest, 
                    mdib_neg_ext_m_retest = pd_mdib_neg_ext_m_retest, 
                    mdib_neg_int_m_conv   = pd_mdib_neg_int_m_conv, 
                    mdib_neg_ext_m_conv   = pd_mdib_neg_ext_m_conv, 
                    mdib_neg_int_m_conc   = pd_mdib_neg_int_m_conc,
                    mdib_neg_ext_m_conc   = pd_mdib_neg_ext_m_conc,
                    mdib_neg_int_m_pred   = pd_mdib_neg_int_m_pred, 
                    mdib_neg_ext_m_pred   = pd_mdib_neg_ext_m_pred)

dir.create("./results")

save(mdib_hd_res, file = "./results/mdib_hd_res.RData")
save(mdib_pd_res, file = "./results/mdib_pd_res.RData")

# ---------------------------------------------------------------------------- #
# Write results ----
# ---------------------------------------------------------------------------- #

# Define function to write results

write_res <- function(results_list) {
  results_list_name <- deparse(substitute(results_list))
  
  sink(paste0("./results/", results_list_name, ".txt"))
  
  print(paste0("Unstandardized Cronbach's alpha and 95% bootstrapped CI for ",
               "'mdib_neg_int' items using baseline data and pairwise deletion:"))
  cat("\n")
  print(round(results_list$mdib_neg_int_bl_alpha, 2))
  cat("\n")
  
  print(paste0("Unstandardized Cronbach's alpha and 95% bootstrapped CI for ",
               "'mdib_neg_ext' items using baseline data and pairwise deletion:"))
  cat("\n")
  print(round(results_list$mdib_neg_ext_bl_alpha, 2))
  cat("\n")
  
  print(paste0("Pearson's correlations for 2-month test-retest reliability using ",
               "average item scores and pairwise deletion:"))
  cat("\n")
  print(rbind(results_list$mdib_neg_int_m_retest,
              results_list$mdib_neg_ext_m_retest), row.names = FALSE)
  cat("\n")
  
  print(paste0("Pearson's correlations for convergent validity at baseline using ",
               "average item scores and pairwise deletion:"))
  cat("\n")
  print(rbind(results_list$mdib_neg_int_m_conv,
              results_list$mdib_neg_ext_m_conv), row.names = FALSE)
  cat("\n")
  
  print(paste0("Pearson's correlations for concurrent validity at baseline using ",
               "average item scores and pairwise deletion:"))
  cat("\n")
  print(rbind(results_list$mdib_neg_int_m_conc,
              results_list$mdib_neg_ext_m_conc), row.names = FALSE)
  cat("\n")
    
  print(paste0("Pearson's correlations for predictive validity from baseline to ",
               "follow-up using average item scores and pairwise deletion:"))
  cat("\n")
  print(rbind(results_list$mdib_neg_int_m_pred,
              results_list$mdib_neg_ext_m_pred), row.names = FALSE)
  
  sink()
}

# Run function

write_res(mdib_hd_res)
write_res(mdib_pd_res)