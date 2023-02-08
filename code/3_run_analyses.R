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
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/mdib/scored/mdib_hd_dat.RData")
load("./data/mdib/scored/mdib_pd_dat.RData")
load("./data/mdib/helper/mdib_dat_items.RData")

# ---------------------------------------------------------------------------- #
# TODO: Compute test-retest reliability for MDIB ----
# ---------------------------------------------------------------------------- #





# ---------------------------------------------------------------------------- #
# Restrict to baseline ----
# ---------------------------------------------------------------------------- #

mdib_hd_dat_bl <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == "baseline_arm_1", ]
mdib_pd_dat_bl <- mdib_pd_dat[mdib_pd_dat$redcap_event_name == "baseline_arm_1", ]

# ---------------------------------------------------------------------------- #
# Compute internal consistency ----
# ---------------------------------------------------------------------------- #

# Compute unstandardized Cronbach's alpha given that items are not standardized 
# prior to computing scale scores (see Falk & Savalei, 2011). Compute both point 
# estimates and CIs (see Iacobucci & Duhachek, 2003).

# Compute bootstrapped CIs for negative MDIB scales at baseline using pairwise 
# deletion (default)

hd_mdib_neg_int_bl_alpha <- psych::alpha(mdib_hd_dat_bl[, mdib_dat_items$mdib_neg_int], n.iter = 1000)$boot.ci
hd_mdib_neg_ext_bl_alpha <- psych::alpha(mdib_hd_dat_bl[, mdib_dat_items$mdib_neg_ext], n.iter = 1000)$boot.ci

pd_mdib_neg_int_bl_alpha <- psych::alpha(mdib_pd_dat_bl[, mdib_dat_items$mdib_neg_int], n.iter = 1000)$boot.ci
pd_mdib_neg_ext_bl_alpha <- psych::alpha(mdib_pd_dat_bl[, mdib_dat_items$mdib_neg_ext], n.iter = 1000)$boot.ci

# ---------------------------------------------------------------------------- #
# Compute correlation between negative bias and anxiety symptoms ----
# ---------------------------------------------------------------------------- #

# Compute Pearson product-moment correlation between negative bias (MDIB) and 
# anxiety symptoms (NeuroQoL) at baseline using listwise deletion (default)

hd_mdib_neg_int_m_bl_cor <- cor.test(mdib_hd_dat_bl$mdib_neg_int_m,
                                     mdib_hd_dat_bl$neuroqol_anx_m, method = "pearson")
hd_mdib_neg_ext_m_bl_cor <- cor.test(mdib_hd_dat_bl$mdib_neg_ext_m,
                                     mdib_hd_dat_bl$neuroqol_anx_m, method = "pearson")

pd_mdib_neg_int_m_bl_cor <- cor.test(mdib_pd_dat_bl$mdib_neg_int_m,
                                     mdib_pd_dat_bl$neuroqol_anx_m, method = "pearson")
pd_mdib_neg_ext_m_bl_cor <- cor.test(mdib_pd_dat_bl$mdib_neg_ext_m,
                                     mdib_pd_dat_bl$neuroqol_anx_m, method = "pearson")

# Compute Pearson product-moment correlation between negative bias (BBSIQ) and 
# anxiety symptoms (NeuroQoL) at baseline using listwise deletion (default)

hd_bbsiq_neg_int_m_bl_cor <- cor.test(mdib_hd_dat_bl$bbsiq_neg_int_m,
                                      mdib_hd_dat_bl$neuroqol_anx_m, method = "pearson")
hd_bbsiq_neg_ext_m_bl_cor <- cor.test(mdib_hd_dat_bl$bbsiq_neg_ext_m,
                                      mdib_hd_dat_bl$neuroqol_anx_m, method = "pearson")

pd_bbsiq_neg_int_m_bl_cor <- cor.test(mdib_pd_dat_bl$bbsiq_neg_int_m,
                                      mdib_pd_dat_bl$neuroqol_anx_m, method = "pearson")
pd_bbsiq_neg_ext_m_bl_cor <- cor.test(mdib_pd_dat_bl$bbsiq_neg_ext_m,
                                      mdib_pd_dat_bl$neuroqol_anx_m, method = "pearson")

# ---------------------------------------------------------------------------- #
# Save results ----
# ---------------------------------------------------------------------------- #

mdib_hd_res <- list(mdib_neg_int_bl_alpha  = hd_mdib_neg_int_bl_alpha,
                    mdib_neg_ext_bl_alpha  = hd_mdib_neg_ext_bl_alpha,
                    mdib_neg_int_m_bl_cor  = hd_mdib_neg_int_m_bl_cor,
                    mdib_neg_ext_m_bl_cor  = hd_mdib_neg_ext_m_bl_cor,
                    bbsiq_neg_int_m_bl_cor = hd_bbsiq_neg_int_m_bl_cor,
                    bbsiq_neg_ext_m_bl_cor = hd_bbsiq_neg_ext_m_bl_cor)

mdib_pd_res <- list(mdib_neg_int_bl_alpha  = pd_mdib_neg_int_bl_alpha,
                    mdib_neg_ext_bl_alpha  = pd_mdib_neg_ext_bl_alpha,
                    mdib_neg_int_m_bl_cor  = pd_mdib_neg_int_m_bl_cor,
                    mdib_neg_ext_m_bl_cor  = pd_mdib_neg_ext_m_bl_cor,
                    bbsiq_neg_int_m_bl_cor = pd_bbsiq_neg_int_m_bl_cor,
                    bbsiq_neg_ext_m_bl_cor = pd_bbsiq_neg_ext_m_bl_cor)

dir.create("./results/mdib", recursive = TRUE)

save(mdib_hd_res, file = "./results/mdib/mdib_hd_res.RData")
save(mdib_pd_res, file = "./results/mdib/mdib_pd_res.RData")

# ---------------------------------------------------------------------------- #
# Write results ----
# ---------------------------------------------------------------------------- #

# Define function to write results

write_res <- function(results_list) {
  results_list_name <- deparse(substitute(results_list))
  
  sink(paste0("./results/mdib/", results_list_name, ".txt"))
  
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
  
  print(paste0("Pearson's correlation between 'mdib_neg_int_m' and 'neuroqol_anx_m' ",
               "total scores using complete baseline data:"))
  print(results_list$mdib_neg_int_m_bl_cor)
  
  print(paste0("Pearson's correlation between 'mdib_neg_ext_m' and 'neuroqol_anx_m' ",
               "total scores using complete baseline data:"))
  print(results_list$mdib_neg_ext_m_bl_cor)
  
  print(paste0("Pearson's correlation between 'bbsiq_neg_int_m' and 'neuroqol_anx_m' ",
               "total scores using complete baseline data:"))
  print(results_list$bbsiq_neg_int_m_bl_cor)
  
  print(paste0("Pearson's correlation between 'bbsiq_neg_ext_m' and 'neuroqol_anx_m' ",
               "total scores using complete baseline data:"))
  print(results_list$bbsiq_neg_ext_m_bl_cor)
  
  sink()
}

# Run function

write_res(mdib_hd_res)
write_res(mdib_pd_res)