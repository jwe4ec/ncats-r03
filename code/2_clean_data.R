# ---------------------------------------------------------------------------- #
# Clean Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import HD and PD MDIB data from RedCap

mdib_hd_dat <- read.csv("./data/mdib/bot_cleaned/final HD Aim 1 data_deid_2023-01-09_1525.csv")
mdib_pd_dat <- read.csv("./data/mdib/bot_cleaned/final PD Aim 1 data_deid_2022-12-08_1339.csv")

# ---------------------------------------------------------------------------- #
# Remove blank rows ----
# ---------------------------------------------------------------------------- #

# For rows where "record_id" is NA, all columns are NA or "". Remove such rows.

sum(is.na(mdib_hd_dat$record_id)) == 237
sum(is.na(mdib_pd_dat$record_id)) == 16

sum(!(is.na(mdib_hd_dat[is.na(mdib_hd_dat$record_id), ]) |
        mdib_hd_dat[is.na(mdib_hd_dat$record_id), ] == "")) == 0
sum(!(is.na(mdib_pd_dat[is.na(mdib_pd_dat$record_id), ]) |
        mdib_pd_dat[is.na(mdib_pd_dat$record_id), ] == "")) == 0

mdib_hd_dat <- mdib_hd_dat[!is.na(mdib_hd_dat$record_id), ]
mdib_pd_dat <- mdib_pd_dat[!is.na(mdib_pd_dat$record_id), ]

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Compare variable names across HD and PD datasets

  # HD dataset has some demographic variables ("cag_repeats", "abnormal_movements", 
  # "age_chorea_onset", "age_diagnosis") and "study_awareness" values (6, 7) in the
  # "demographic_hd_history" form that are not applicable to PD participants

setdiff(names(mdib_hd_dat), names(mdib_pd_dat))

  # PD dataset has some demographic variables ("age_diagnosis_pd", "pd_medication",
  # "wearingoff") in the "demographic_pd_history" form that are not applicable to
  # HD participants. PD dataset also has extra form: "mds_updrs_part_ii".

setdiff(names(mdib_pd_dat), names(mdib_hd_dat))

# Confirm variable names for interpretation bias (MDIB, BBSIQ) and anxiety symptoms 
# (NeuroQoL) are same across HD and PD datasets

all(names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("md_bbsiq", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("bbsiq_", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("bbsiq", names(mdib_pd_dat)) & !grepl("md_bbsiq", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))] ==
      names(mdib_pd_dat)[grepl("neuroqol", names(mdib_pd_dat)) & !(grepl("complete", names(mdib_pd_dat)))])

# Define items for negative bias (MDIB, BBSIQ) and anxiety symptoms (NeuroQoL)

mdib_neg_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("neg", names(mdib_hd_dat))]

  # Note: The object for BBSIQ items is appended with "mdib" because prior analyses
  # (https://github.com/jwe4ec/pa-20-206) found that BBSIQ item names differ between
  # the MDIB and MindTrails-HD Data Server datasets. Although the MT-HD dataset is
  # not relevant to the present analyses, we retain the label for clarity.

bbsiq_neg_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("neg", names(mdib_hd_dat))]

neuroqol_anx_items <-
  names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))

all(bbsiq_neg_items_mdib == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", 
                              "bbsiq_4c_neg", "bbsiq_5a_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_8c_neg", "bbsiq_9b_neg", 
                              "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_13c_neg", "bbsiq_14c_neg"))

all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

length(mdib_neg_items) == 12
length(bbsiq_neg_items_mdib) == 14
length(neuroqol_anx_items) == 8

# Define items for purported MDIB scales (internal threats = catastrophizing 
# about disease progression, external threats = negative social evaluation) and 
# BBSIQ scales (internal threats, external threats)

mdib_neg_int_items <- c("md_bbsiq_1b_neg", "md_bbsiq_4c_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_8b_neg", "md_bbsiq_12b_neg")
mdib_neg_ext_items <- c("md_bbsiq_2a_neg", "md_bbsiq_3c_neg", "md_bbsiq_5a_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_9c_neg", "md_bbsiq_10a_neg", 
                        "md_bbsiq_11c_neg")

length(mdib_neg_int_items) == 5
length(mdib_neg_ext_items) == 7

bbsiq_neg_int_items_mdib <- c("bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_5a_neg", 
                              "bbsiq_8c_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_14c_neg")
bbsiq_neg_ext_items_mdib <- c("bbsiq_1c_neg", "bbsiq_4c_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_9b_neg", "bbsiq_10b_neg", 
                              "bbsiq_13c_neg")

length(bbsiq_neg_int_items_mdib) == 7
length(bbsiq_neg_ext_items_mdib) == 7

# Store items in list

mdib_dat_items <- list(mdib_neg = mdib_neg_items,
                       mdib_neg_int = mdib_neg_int_items,
                       mdib_neg_ext = mdib_neg_ext_items,
                       bbsiq_neg = bbsiq_neg_items_mdib,
                       bbsiq_neg_int = bbsiq_neg_int_items_mdib,
                       bbsiq_neg_ext = bbsiq_neg_ext_items_mdib,
                       neuroqol_anx = neuroqol_anx_items)

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) as NA

target_items <- c(mdib_dat_items$mdib_neg, 
                  mdib_dat_items$bbsiq_neg, 
                  mdib_dat_items$neuroqol_anx)

mdib_hd_dat[, target_items][mdib_hd_dat[, target_items] == 99] <- NA
mdib_pd_dat[, target_items][mdib_pd_dat[, target_items] == 99] <- NA

# ---------------------------------------------------------------------------- #
# Score scales ----
# ---------------------------------------------------------------------------- #

# Compute mean of available items

mdib_hd_dat$mdib_neg_int_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_int],  na.rm = TRUE)
mdib_hd_dat$mdib_neg_ext_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_ext],  na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_int_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_int], na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_ext_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_ext], na.rm = TRUE)
mdib_hd_dat$neuroqol_anx_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$neuroqol_anx],  na.rm = TRUE)

mdib_pd_dat$mdib_neg_int_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$mdib_neg_int],  na.rm = TRUE)
mdib_pd_dat$mdib_neg_ext_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$mdib_neg_ext],  na.rm = TRUE)
mdib_pd_dat$bbsiq_neg_int_m <- rowMeans(mdib_pd_dat[, mdib_dat_items$bbsiq_neg_int], na.rm = TRUE)
mdib_pd_dat$bbsiq_neg_ext_m <- rowMeans(mdib_pd_dat[, mdib_dat_items$bbsiq_neg_ext], na.rm = TRUE)
mdib_pd_dat$neuroqol_anx_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$neuroqol_anx],  na.rm = TRUE)

mdib_hd_dat$mdib_neg_int_m[is.nan(mdib_hd_dat$mdib_neg_int_m)]   <- NA
mdib_hd_dat$mdib_neg_ext_m[is.nan(mdib_hd_dat$mdib_neg_ext_m)]   <- NA
mdib_hd_dat$bbsiq_neg_int_m[is.nan(mdib_hd_dat$bbsiq_neg_int_m)] <- NA
mdib_hd_dat$bbsiq_neg_ext_m[is.nan(mdib_hd_dat$bbsiq_neg_ext_m)] <- NA
mdib_hd_dat$neuroqol_anx_m[is.nan(mdib_hd_dat$neuroqol_anx_m)]   <- NA

mdib_pd_dat$mdib_neg_int_m[is.nan(mdib_pd_dat$mdib_neg_int_m)]   <- NA
mdib_pd_dat$mdib_neg_ext_m[is.nan(mdib_pd_dat$mdib_neg_ext_m)]   <- NA
mdib_pd_dat$bbsiq_neg_int_m[is.nan(mdib_pd_dat$bbsiq_neg_int_m)] <- NA
mdib_pd_dat$bbsiq_neg_ext_m[is.nan(mdib_pd_dat$bbsiq_neg_ext_m)] <- NA
mdib_pd_dat$neuroqol_anx_m[is.nan(mdib_pd_dat$neuroqol_anx_m)]   <- NA

# Compute total scores based on mean of available items

mdib_hd_dat$mdib_neg_int_tot  <- mdib_hd_dat$mdib_neg_int_m  * length(mdib_dat_items$mdib_neg_int)
mdib_hd_dat$mdib_neg_ext_tot  <- mdib_hd_dat$mdib_neg_ext_m  * length(mdib_dat_items$mdib_neg_ext)
mdib_hd_dat$bbsiq_neg_int_tot <- mdib_hd_dat$bbsiq_neg_int_m * length(mdib_dat_items$bbsiq_neg_int)
mdib_hd_dat$bbsiq_neg_ext_tot <- mdib_hd_dat$bbsiq_neg_ext_m * length(mdib_dat_items$bbsiq_neg_ext)
mdib_hd_dat$neuroqol_anx_tot  <- mdib_hd_dat$neuroqol_anx_m  * length(mdib_dat_items$neuroqol_anx)

mdib_pd_dat$mdib_neg_int_tot  <- mdib_pd_dat$mdib_neg_int_m  * length(mdib_dat_items$mdib_neg_int)
mdib_pd_dat$mdib_neg_ext_tot  <- mdib_pd_dat$mdib_neg_ext_m  * length(mdib_dat_items$mdib_neg_ext)
mdib_pd_dat$bbsiq_neg_int_tot <- mdib_pd_dat$bbsiq_neg_int_m * length(mdib_dat_items$bbsiq_neg_int)
mdib_pd_dat$bbsiq_neg_ext_tot <- mdib_pd_dat$bbsiq_neg_ext_m * length(mdib_dat_items$bbsiq_neg_ext)
mdib_pd_dat$neuroqol_anx_tot  <- mdib_pd_dat$neuroqol_anx_m  * length(mdib_dat_items$neuroqol_anx)

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/mdib/scored")

save(mdib_hd_dat, file = "./data/mdib/scored/mdib_hd_dat.RData")
save(mdib_pd_dat, file = "./data/mdib/scored/mdib_pd_dat.RData")

dir.create("./data/mdib/helper")

save(mdib_dat_items, file = "./data/mdib/helper/mdib_dat_items.RData")