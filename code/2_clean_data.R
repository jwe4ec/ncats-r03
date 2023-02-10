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

# Confirm variable names for interpretation bias (MDIB, BBSIQ), anxiety sensitivity
# (ASI), fear of negative evaluation (BFNE-II), anxiety symptoms (Neuro-QoL Anxiety), 
# and social avoidance and distress (full SADS at baseline and reduced subset of SADS 
# items we decided to administer at follow-up) are same across HD and PD datasets

all(names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("md_bbsiq", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("bbsiq_", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("bbsiq", names(mdib_pd_dat)) & !grepl("md_bbsiq", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("asi_", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("asi_", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("bfne_", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("bfne_", names(mdib_pd_dat))])

all(names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))] ==
      names(mdib_pd_dat)[grepl("neuroqol", names(mdib_pd_dat)) & !(grepl("complete", names(mdib_pd_dat)))])

all(names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & !(grepl("_v2", names(mdib_hd_dat)))] == 
      names(mdib_pd_dat)[grepl("sad_", names(mdib_pd_dat)) & !(grepl("_v2", names(mdib_pd_dat)))])

all(names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & grepl("_v2", names(mdib_hd_dat))] == 
      names(mdib_pd_dat)[grepl("sad_", names(mdib_pd_dat)) & grepl("_v2", names(mdib_pd_dat))])

# Define items for negative bias (MDIB, BBSIQ), anxiety sensitivity (ASI), fear of 
# negative evaluation (BFNE-II), anxiety symptoms (Neuro-QoL Anxiety), and social 
# avoidance and distress (full SADS at baseline and reduced SADS at follow-up)

mdib_neg_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("neg", names(mdib_hd_dat))]

  # Note: The object for BBSIQ items is appended with "mdib" because prior analyses
  # (https://github.com/jwe4ec/pa-20-206) found that BBSIQ item names differ between
  # the MDIB and MindTrails-HD Data Server datasets. Although the MT-HD dataset is
  # not relevant to the present analyses, we retain the label for clarity.

bbsiq_neg_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("neg", names(mdib_hd_dat))]

asi_items <- names(mdib_hd_dat)[grepl("asi_", names(mdib_hd_dat))]

bfne2_items <- names(mdib_hd_dat)[grepl("bfne_", names(mdib_hd_dat))]

neuroqol_anx_items <-
  names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))]

sads_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & !(grepl("_v2", names(mdib_hd_dat)))]

sads_red_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & grepl("_v2", names(mdib_hd_dat))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))

all(bbsiq_neg_items_mdib == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", 
                              "bbsiq_4c_neg", "bbsiq_5a_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_8c_neg", "bbsiq_9b_neg", 
                              "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_13c_neg", "bbsiq_14c_neg"))

all(asi_items == c("asi_1", "asi_2", "asi_3", "asi_4", "asi_5", "asi_6", "asi_7", 
                   "asi_8", "asi_9", "asi_10", "asi_11", "asi_12", "asi_13", "asi_14", 
                   "asi_15", "asi_16"))

all(bfne2_items == c("bfne_1", "bfne_2", "bfne_3", "bfne_4", "bfne_5", "bfne_6", 
                     "bfne_7", "bfne_8", "bfne_9", "bfne_10", "bfne_11", "bfne_12"))

all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

all(sads_items == c("sad_1", "sad_2", "sad_3", "sad_4", "sad_5", "sad_6", "sad_7", 
                    "sad_8", "sad_9", "sad_10", "sad_11", "sad_12", "sad_13", "sad_14", 
                    "sad_15", "sad_16", "sad_17", "sad_18", "sad_19", "sad_20", "sad_21", 
                    "sad_22", "sad_23", "sad_24", "sad_25", "sad_26", "sad_27", "sad_28"))

all(sads_red_items == c("sad_20_v2", "sad_27_v2", "sad_13_v2", "sad_12_v2", 
                        "sad_24_v2", "sad_15_v2", "sad_4_v2", "sad_16_v2"))

length(mdib_neg_items) == 12
length(bbsiq_neg_items_mdib) == 14
length(asi_items) == 16
length(bfne2_items) == 12
length(neuroqol_anx_items) == 8
length(sads_items) == 28
length(sads_red_items) == 8

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

# Define items for 8-item BFNE-II, which is preferred (see Carleton et al., 2007; 
# https://doi.org/bgn7v6)

bfne2_8_items <- bfne2_items[!(bfne2_items %in% c("bfne_2", "bfne_4", 
                                                  "bfne_7", "bfne_11"))]

all(bfne2_8_items == c("bfne_1", "bfne_3", "bfne_5", "bfne_6", "bfne_8", "bfne_9", 
                       "bfne_10", "bfne_12"))

length(bfne2_8_items) == 8

# Store items in list

mdib_dat_items <- list(mdib_neg = mdib_neg_items,
                       mdib_neg_int = mdib_neg_int_items,
                       mdib_neg_ext = mdib_neg_ext_items,
                       bbsiq_neg = bbsiq_neg_items_mdib,
                       bbsiq_neg_int = bbsiq_neg_int_items_mdib,
                       bbsiq_neg_ext = bbsiq_neg_ext_items_mdib,
                       asi = asi_items,
                       bfne2 = bfne2_items,
                       bfne2_8 = bfne2_8_items,
                       neuroqol_anx = neuroqol_anx_items,
                       sads = sads_items,
                       sads_red = sads_red_items)

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) as NA

target_items <- c(mdib_dat_items$mdib_neg, 
                  mdib_dat_items$bbsiq_neg,
                  mdib_dat_items$asi,
                  mdib_dat_items$bfne2,
                  mdib_dat_items$neuroqol_anx,
                  mdib_dat_items$sads,
                  mdib_dat_items$sads_red)

mdib_hd_dat[, target_items][mdib_hd_dat[, target_items] == 99] <- NA
mdib_pd_dat[, target_items][mdib_pd_dat[, target_items] == 99] <- NA

# ---------------------------------------------------------------------------- #
# Recode BFNE-II ----
# ---------------------------------------------------------------------------- #

# Given that Carleton et al. (2007) uses a 0-4 scale, recode our 1-5 scale to a
# 0-4 scale (response options on REDCap did not have numbers listed next to them)

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(1, 5))
all(range(mdib_pd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(1, 5))

mdib_hd_dat[, mdib_dat_items$bfne2] <- mdib_hd_dat[, mdib_dat_items$bfne2] - 1
mdib_pd_dat[, mdib_dat_items$bfne2] <- mdib_pd_dat[, mdib_dat_items$bfne2] - 1

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(0, 4))
all(range(mdib_pd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(0, 4))

# ---------------------------------------------------------------------------- #
# Score scales ----
# ---------------------------------------------------------------------------- #

# Compute mean of available items

mdib_hd_dat$mdib_neg_int_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_int],  na.rm = TRUE)
mdib_hd_dat$mdib_neg_ext_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_ext],  na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_int_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_int], na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_ext_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_ext], na.rm = TRUE)
mdib_hd_dat$asi_m           <- rowMeans(mdib_hd_dat[, mdib_dat_items$asi],           na.rm = TRUE)
mdib_hd_dat$bfne2_8_m       <- rowMeans(mdib_hd_dat[, mdib_dat_items$bfne2_8],       na.rm = TRUE)
mdib_hd_dat$neuroqol_anx_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$neuroqol_anx],  na.rm = TRUE)
mdib_hd_dat$sads_red_m      <- rowMeans(mdib_hd_dat[, mdib_dat_items$sads_red],      na.rm = TRUE)

mdib_pd_dat$mdib_neg_int_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$mdib_neg_int],  na.rm = TRUE)
mdib_pd_dat$mdib_neg_ext_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$mdib_neg_ext],  na.rm = TRUE)
mdib_pd_dat$bbsiq_neg_int_m <- rowMeans(mdib_pd_dat[, mdib_dat_items$bbsiq_neg_int], na.rm = TRUE)
mdib_pd_dat$bbsiq_neg_ext_m <- rowMeans(mdib_pd_dat[, mdib_dat_items$bbsiq_neg_ext], na.rm = TRUE)
mdib_pd_dat$asi_m           <- rowMeans(mdib_pd_dat[, mdib_dat_items$asi],           na.rm = TRUE)
mdib_pd_dat$bfne2_8_m       <- rowMeans(mdib_pd_dat[, mdib_dat_items$bfne2_8],       na.rm = TRUE)
mdib_pd_dat$neuroqol_anx_m  <- rowMeans(mdib_pd_dat[, mdib_dat_items$neuroqol_anx],  na.rm = TRUE)
mdib_pd_dat$sads_red_m      <- rowMeans(mdib_pd_dat[, mdib_dat_items$sads_red],      na.rm = TRUE)

mdib_hd_dat$mdib_neg_int_m[is.nan(mdib_hd_dat$mdib_neg_int_m)]   <- NA
mdib_hd_dat$mdib_neg_ext_m[is.nan(mdib_hd_dat$mdib_neg_ext_m)]   <- NA
mdib_hd_dat$bbsiq_neg_int_m[is.nan(mdib_hd_dat$bbsiq_neg_int_m)] <- NA
mdib_hd_dat$bbsiq_neg_ext_m[is.nan(mdib_hd_dat$bbsiq_neg_ext_m)] <- NA
mdib_hd_dat$asi_m[is.nan(mdib_hd_dat$asi_m)]                     <- NA
mdib_hd_dat$bfne2_8_m[is.nan(mdib_hd_dat$bfne2_8_m)]             <- NA
mdib_hd_dat$neuroqol_anx_m[is.nan(mdib_hd_dat$neuroqol_anx_m)]   <- NA
mdib_hd_dat$sads_red_m[is.nan(mdib_hd_dat$sads_red_m)]           <- NA

mdib_pd_dat$mdib_neg_int_m[is.nan(mdib_pd_dat$mdib_neg_int_m)]   <- NA
mdib_pd_dat$mdib_neg_ext_m[is.nan(mdib_pd_dat$mdib_neg_ext_m)]   <- NA
mdib_pd_dat$bbsiq_neg_int_m[is.nan(mdib_pd_dat$bbsiq_neg_int_m)] <- NA
mdib_pd_dat$bbsiq_neg_ext_m[is.nan(mdib_pd_dat$bbsiq_neg_ext_m)] <- NA
mdib_pd_dat$asi_m[is.nan(mdib_pd_dat$asi_m)]                     <- NA
mdib_pd_dat$bfne2_8_m[is.nan(mdib_pd_dat$bfne2_8_m)]             <- NA
mdib_pd_dat$neuroqol_anx_m[is.nan(mdib_pd_dat$neuroqol_anx_m)]   <- NA
mdib_pd_dat$sads_red_m[is.nan(mdib_pd_dat$sads_red_m)]           <- NA

# Compute total scores based on mean of available items

mdib_hd_dat$mdib_neg_int_tot  <- mdib_hd_dat$mdib_neg_int_m  * length(mdib_dat_items$mdib_neg_int)
mdib_hd_dat$mdib_neg_ext_tot  <- mdib_hd_dat$mdib_neg_ext_m  * length(mdib_dat_items$mdib_neg_ext)
mdib_hd_dat$bbsiq_neg_int_tot <- mdib_hd_dat$bbsiq_neg_int_m * length(mdib_dat_items$bbsiq_neg_int)
mdib_hd_dat$bbsiq_neg_ext_tot <- mdib_hd_dat$bbsiq_neg_ext_m * length(mdib_dat_items$bbsiq_neg_ext)
mdib_hd_dat$asi_tot           <- mdib_hd_dat$asi_m           * length(mdib_dat_items$asi)
mdib_hd_dat$bfne2_8_tot       <- mdib_hd_dat$bfne2_8_m       * length(mdib_dat_items$bfne2_8)
mdib_hd_dat$neuroqol_anx_tot  <- mdib_hd_dat$neuroqol_anx_m  * length(mdib_dat_items$neuroqol_anx)
mdib_hd_dat$sads_red_tot      <- mdib_hd_dat$sads_red_m      * length(mdib_dat_items$sads_red)

mdib_pd_dat$mdib_neg_int_tot  <- mdib_pd_dat$mdib_neg_int_m  * length(mdib_dat_items$mdib_neg_int)
mdib_pd_dat$mdib_neg_ext_tot  <- mdib_pd_dat$mdib_neg_ext_m  * length(mdib_dat_items$mdib_neg_ext)
mdib_pd_dat$bbsiq_neg_int_tot <- mdib_pd_dat$bbsiq_neg_int_m * length(mdib_dat_items$bbsiq_neg_int)
mdib_pd_dat$bbsiq_neg_ext_tot <- mdib_pd_dat$bbsiq_neg_ext_m * length(mdib_dat_items$bbsiq_neg_ext)
mdib_pd_dat$asi_tot           <- mdib_pd_dat$asi_m           * length(mdib_dat_items$asi)
mdib_pd_dat$bfne2_8_tot       <- mdib_pd_dat$bfne2_8_m       * length(mdib_dat_items$bfne2_8)
mdib_pd_dat$neuroqol_anx_tot  <- mdib_pd_dat$neuroqol_anx_m  * length(mdib_dat_items$neuroqol_anx)
mdib_pd_dat$sads_red_tot      <- mdib_pd_dat$sads_red_m      * length(mdib_dat_items$sads_red)

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/mdib/scored")

save(mdib_hd_dat, file = "./data/mdib/scored/mdib_hd_dat.RData")
save(mdib_pd_dat, file = "./data/mdib/scored/mdib_pd_dat.RData")

dir.create("./data/mdib/helper")

save(mdib_dat_items, file = "./data/mdib/helper/mdib_dat_items.RData")