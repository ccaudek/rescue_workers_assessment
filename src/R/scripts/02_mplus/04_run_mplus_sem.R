# Script name: 04_run_mplus_sem.R
# Project: rescue-workers
# Script purpose: SEM with Mplus models 4b and 5b, two groups, same measurement
# model, different structural coefficients.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Oct 24 08:48:53 2022
# Last Modified Date: Thu Oct 27 15:31:22 2022
# 
# Notes: 


library("here")
library("tidyverse")
library("MplusAutomation")
library("gt")
library("glue")
library("kableExtra")
library("misty")
library("lavaan")
library("AICcmodavg")
library("nonnest2")
library("DiagrammeR")

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6 # Use 6 cores
)

source(here::here("src", "R", "functions", "funs_add_neoffi60_subscales.R"))
source(here::here("src", "R", "functions", "funs_correct_iesr_scores.R"))
source(here::here("src", "R", "functions", "funs_plot_job_qualification.R"))
source(here::here("src", "R", "functions", "funs_generate_all_items_df.R"))

scale_this <- function(x) as.vector(scale(x))

# Get data.
all_items <- generate_all_items_df()

# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here(
  "src", "R", "scripts", "02_mplus", "mplus_models", "sem/"
)


# Save data for Mplus -----------------------------------------------------

d2 <- all_items

d2$grp <- ifelse(as.character(d2$group) == "rescue_workers", 0, 1)

d2 |> 
  group_by(grp) |> 
  summarise(
    n = n()
  )

sem_df <- d2 |> 
  dplyr::select(
    # SCS
    all_of(contains("scs_")), 
    pos_sc, neg_sc, ts_sc,
    sk, ch, mi, sj, is, oi,
    # PTGI
    appreciation_of_life, new_possibilities, personal_strength, spirituality, 
    relating_to_others,
    # IES-R
    avoiding, intrusivity, hyperarousal,
    # COPE-NVI
    positive_attitude, problem_orientation,
    # MSPSS
    family, friends, significant_other,
    # NEO-FFI-60 Neuroticism
    negative_affect, self_reproach,
    # NEO-FFI-60 Extraversion
    positive_affect, sociability, activity,
    # group
    grp
  )


# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  sem_df, 
  file = here::here(
    "src", "R", "scripts", "02_mplus", "mplus_models", "sem", "semdatass.dat"
  )
)


# Run Mplus models --------------------------------------------------------


# Mediation model nn_m4b - two separate groups ----------------------------

MplusAutomation::runModels(paste0(the_dir, "/nn_m4b.inp"), showOutput = TRUE)
nn_m4b <- MplusAutomation::readModels(paste0(the_dir, "/nn_m4b.out"))
summary(nn_m4b)
# Model 4b: Bifactor-ESEM (1 G- and 6 S-factors). Two groups. 
# Same measurement model, different structural coefficients.Estimated using MLR 
# Number of obs: 1068, number of (free) parameters: 434 
# 
# Model: Chi2(df = 1370) = 2529.245, p = 0 
# Baseline model: Chi2(df = 1640) = 21623.244, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.942, TLI = 0.931, SRMR = 0.041 
# RMSEA = 0.04, 90% CI [0.037, 0.042], p < .05 = 1 
# AIC = 161489.581, BIC = 163648.099 


# Read factor scores.
fs <- read.table(paste0(the_dir, "/nn_m4b_scores.txt"))
# The order of the FS columns is reported in the mplus.out file.

sk <- rowSums(fs[, c(5, 12, 19, 23, 26)])
y <- fs$V44  
# plot(y, sk)
cor.test(y, sk)
# 0.4474024 [0.3981004 0.4941273]

sj <- rowSums(fs[, c(1, 8, 11, 16, 21)])
y <- fs$V46  
# plot(y, sj)
cor.test(y, sj)
# 0.9100888 [0.8991920 0.9198575]

ch <- rowSums(fs[, c(3, 7, 10, 15)])
y <- fs$V48  
# plot(y, ch)
cor.test(y, ch)
# 0.7826189 [0.7582288 0.8048217]

is <- rowSums(fs[, c(4, 13, 18, 25)])
y <- fs$V50  
# plot(y, is)
cor.test(y, is)
# 0.9487687 [0.9424183 0.9544351]

mi <- rowSums(fs[, c(9, 14, 17, 22)])
y <- fs$V52  
# plot(y, mi)
cor.test(y, mi)
# 0.8257281 [0.8056475 0.8439133]

oi <- rowSums(fs[, c(2, 6, 20, 24)])
y <- fs$V54 
# plot(y, oi)
cor.test(y, oi)
# 0.9342233 [0.9261383 0.9414501]

# Negative self-compassion.
cs_n <- sj + is + oi
y <- fs$V42
# plot(y, cs_n)
cor.test(y, cs_n)
# -0.5367324 [-0.5781056 -0.4926064]

# Positive self-compassion.
cs_p <- sk + ch + mi
y <- fs$V42
# plot(y, cs_p)
cor.test(y, cs_p)
# 0.9014708 [0.8895899 0.9121327]

# Self-compassion total score.
cs <- sj + is + oi + sk + ch + mi
y <- fs$V42
# plot(y, cs)
cor.test(y, cs)
# 0.1397573  [0.08044548 0.19808289]



# Mediation model m5b  ------------------------------------------------


# Model 5b RW

# Only for the RW group -- the model with two groups does not converge.
MplusAutomation::runModels(paste0(the_dir, "/nn_m5b.inp"), showOutput = TRUE)
nn_m5b <- MplusAutomation::readModels(paste0(the_dir, "/nn_m5b.out"))
summary(nn_m5b)
# Model 5b: Bifactor-ESEM (1 G- and 6 S-factors).Estimated using MLR 
# Number of obs: 746, number of (free) parameters: 336 
# 
# Model: Chi2(df = 698) = 1298.996, p = 0 
# Baseline model: Chi2(df = 946) = 15688.075, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.959, TLI = 0.945, SRMR = 0.031 
# RMSEA = 0.034, 90% CI [0.031, 0.037], p < .05 = 1 
# AIC = 124720.106, BIC = 126270.654 


# Read factor scores.
m5b_rw_fs <- read.table(paste0(the_dir, "/nn_m5b_scores.txt"))
# The order of the FS columns is reported in the mplus.out file.

sk <- rowSums(m5b_rw_fs[, c(5, 12, 19, 23, 26)])
sj <- rowSums(m5b_rw_fs[, c(1, 8, 11, 16, 21)])
ch <- rowSums(m5b_rw_fs[, c(3, 7, 10, 15)])
is <- rowSums(m5b_rw_fs[, c(4, 13, 18, 25)])
mi <- rowSums(m5b_rw_fs[, c(9, 14, 17, 22)])
oi <- rowSums(m5b_rw_fs[, c(2, 6, 20, 24)])

cor.test(m5b_rw_fs$V45, sk)
# 0.7124553 [0.6752050 0.7460809]

cor.test(m5b_rw_fs$V47, sj)
# 0.931247 [0.9210333 0.9401808]

cor.test(m5b_rw_fs$V49, ch)
# 0.8998301 [0.8852267 0.9126616]

cor.test(m5b_rw_fs$V51, is)
# 0.9580004 [0.9516617 0.9635235]

cor.test(m5b_rw_fs$V53, mi)
# -0.01155331 [-0.08326470  0.06027711]

cor.test(m5b_rw_fs$V55, oi)
# 0.870474 [0.8519243 0.8868419]

cs_p <- sk + ch + mi
cs_n <- sj + is + oi

# V57 PO 
# V59 NE

# Positive latent self-compassion.
y <- m5b_rw_fs$V57
cor.test(y, cs_p)
# -0.6763005 [-0.7134466 -0.6353639]

# Negative latent self-compassion.
y <- m5b_rw_fs$V59
cor.test(y, cs_n)
# 0.2057244 [0.1359516 0.2734666]


# Model 5b does not converge for the control group.



# Model nn1 --------------------------------------------------------------

## In the manuscript, this model is called nn1.

# Only RW.
runModels(paste0(the_dir, "/nn_m6.inp"), showOutput = TRUE)
nn_m6 <- MplusAutomation::readModels(paste0(the_dir, "/nn_m6.out"))
# summary(nn_m6)
# Model 6, RW.Estimated using MLR 
# Number of obs: 746, number of (free) parameters: 100 
# 
# Model: Chi2(df = 224) = 730.471, p = 0 
# Baseline model: Chi2(df = 276) = 8560.748, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.939, TLI = 0.925, SRMR = 0.054 
# RMSEA = 0.055, 90% CI [0.051, 0.06], p < .05 = 0.03 
# AIC = 95740.677, BIC = 96202.149 


# Model nn2 --------------------------------------------------------------

## In the manuscript, this model is called nn2.

# Only RW.
runModels(paste0(the_dir, "/nn_m9.inp"), showOutput = TRUE)
nn_m9 <- MplusAutomation::readModels(paste0(the_dir, "/nn_m9.out"))
summary(nn_m9)
# Model 9, RW.Estimated using MLR 
# Number of obs: 746, number of (free) parameters: 97 
# 
# Model: Chi2(df = 155) = 658.311, p = 0 
# Baseline model: Chi2(df = 210) = 7856.202, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.934, TLI = 0.911, SRMR = 0.04 
# RMSEA = 0.066, 90% CI [0.061, 0.071], p < .05 = 0 
# AIC = 82755.397, BIC = 83203.025 


# Model nn3 --------------------------------------------------------------

# Single factor model identified by the six SCS subscales.

runModels(paste0(the_dir, "/nn_m7.inp"), showOutput = TRUE)
nn_m7 <- MplusAutomation::readModels(paste0(the_dir, "/nn_m7.out"))
summary(nn_m7)
# Model 7, RW.Estimated using MLR 
# Number of obs: 746, number of (free) parameters: 78 
# 
# Model: Chi2(df = 174) = 1281.57, p = 0 
# Baseline model: Chi2(df = 210) = 7856.202, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.855, TLI = 0.825, SRMR = 0.09 
# RMSEA = 0.092, 90% CI [0.088, 0.097], p < .05 = 0 
# AIC = 83397.756, BIC = 83757.705 



# Model nn4 --------------------------------------------------------------

# Hierarchical version of model nn2.

runModels(paste0(the_dir, "/nn_m8.inp"), showOutput = TRUE)
nn_m8 <- MplusAutomation::readModels(paste0(the_dir, "/nn_m8.out"))
summary(nn_m8)
# Model 8, RW.Estimated using MLR 
# Number of obs: 746, number of (free) parameters: 80 
# 
# Model: Chi2(df = 172) = 859.754, p = 0 
# Baseline model: Chi2(df = 210) = 7856.202, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.91, TLI = 0.89, SRMR = 0.08 
# RMSEA = 0.073, 90% CI [0.068, 0.078], p < .05 = 0 
# AIC = 82944.682, BIC = 83313.86 








####### The models below have not been used.


# Both groups.
runModels(paste0(the_dir, "/nn_m6b.inp"), showOutput = TRUE)
nn_m6b <- MplusAutomation::readModels(paste0(the_dir, "/nn_m6b.out"))
summary(nn_m6b)
# Model: Chi2(df = 482) = 1883.08, p = 0 
# Baseline model: Chi2(df = 552) = 12190.894, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.88, TLI = 0.862, SRMR = 0.063 
# RMSEA = 0.074, 90% CI [0.07, 0.077], p < .05 = 0 
# AIC = 138536.14, BIC = 139361.748 


# Model 7 RW --------------------------------------------------------------



# Model 8 RW --------------------------------------------------------------












# Model m4b - both groups together ----------------------------------------

MplusAutomation::runModels(paste0(the_dir, "/m4b.inp"), showOutput = TRUE)
m4b <- MplusAutomation::readModels(paste0(the_dir, "/m4b.out"))
summary(m4b)
# Number of obs: 1068, number of (free) parameters: 265 
# Model: Chi2(df = 164) = 542.343, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
# CFI = 0.987, TLI = 0.975, SRMR = 0.014 
# RMSEA = 0.046, 90% CI [0.042, 0.051], p < .05 = 0.907 
# AIC = NA, BIC = NA 

fs <- read.table(
  paste0(the_dir, "/m4b_scores.txt")
)

sk <- rowSums(fs[, c(5, 12, 19, 23, 26)])
y <- fs$V29  # 29 sk --
plot(y, sk)
cor(y, sk)

sj <- rowSums(fs[, c(1, 8, 11, 16, 21)])
y <- fs$V31  # 27 sj --
plot(y, sj)
cor(y, sj)

ch <- rowSums(fs[, c(3, 7, 10, 15)])
y <- fs$V33  # 33 ch --
plot(y, ch)
cor(y, ch)

is <- rowSums(fs[, c(4, 13, 18, 25)])
y <- fs$V35  #  35 is --
plot(y, is)
cor(y, is)

mi <- rowSums(fs[, c(9, 14, 17, 22)])
y <- fs$V37  #  37 is --
plot(y, mi)
cor(y, mi)

oi <- rowSums(fs[, c(2, 6, 20, 24)])
y <- fs$V39  #  39 is
plot(y, oi)
cor(y, oi)

cs_n <- sj + is + oi
y <- fs$V27
plot(y, cs_n)
cor(y, cs_n)

cs_p <- sk + ch + mi
y <- fs$V27
plot(y, cs_p)
cor(y, cs_p)

cs <- sj + is + oi + sk + ch + mi
y <- fs$V27
plot(y, cs)
cor(y, cs)

# e o f ----



# Compute SCS TS, SCS positive and SCS negative scores --------------------
# 
# scs_pos_items <- d2 |> 
#   dplyr::select(
#     scs_5, scs_12, scs_19, scs_23, scs_26,
#     scs_3, scs_7, scs_10, scs_15,
#     scs_9, scs_14, scs_17, scs_22
#   )
# d2$pos_sc <- rowSums(scs_pos_items)
# 
# scs_neg_items <- d2 |> 
#   dplyr::select(
#     scs_1, scs_8, scs_11, scs_16, scs_21,
#     scs_4, scs_13, scs_18, scs_25,
#     scs_2, scs_6, scs_20, scs_24
#   )
# d2$neg_sc <- rowSums(scs_neg_items)
# 
# # SCS TS
# d2$ts_sc <- d2$pos_sc + d2$neg_sc
# 
# # SCS subscales.
# sk_df <- d2 |> 
#   dplyr::select(
#     scs_5, scs_12, scs_19, scs_23, scs_26
#   )
# d2$sk <- rowSums(sk_df)
# 
# ch_df <- d2 |> 
#   dplyr::select(
#     scs_3, scs_7, scs_10, scs_15
#   )
# d2$ch <- rowSums(ch_df)
# 
# mi_df <- d2 |> 
#   dplyr::select(
#     scs_9, scs_14, scs_17, scs_22
#   )
# d2$mi <- rowSums(mi_df)
# 
# sj_df <- d2 |> 
#   dplyr::select(
#     scs_1, scs_8, scs_11, scs_16, scs_21
#   )
# d2$sj <- rowSums(sj_df)
# 
# is_df <- d2 |> 
#   dplyr::select(
#     scs_4, scs_13, scs_18, scs_25
#   )
# d2$is <- rowSums(is_df)
# 
# oi_df <- d2 |> 
#   dplyr::select(
#     scs_2, scs_6, scs_20, scs_24
#   )
# d2$oi <- rowSums(oi_df)
# 
# 
# d2 |> 
#   group_by(grp) |> 
#   summarise(
#     avg_cs_p = mean(pos_sc),
#     avg_cs_n = mean(neg_sc),
#     avg_sk = mean(sk),
#     avg_ch = mean(ch),
#     avg_mi = mean(mi),
#     avg_sj = mean(sj),
#     avg_is = mean(is),
#     avg_oi = mean(oi),
#     n = n()
#   )

# temp <- tibble(
#   sk, ch, mi, sj, is, oi, grp = d2$grp
# )
# 
# psych::alpha(d2$sk)


# t.test(sk ~ grp, d2)
# t.test(ch ~ grp, d2)
# t.test(mi ~ grp, d2)
# 
# t.test(sj ~ grp, d2)
# t.test(is ~ grp, d2)
# t.test(oi ~ grp, d2)
# 
# cohens_d(sk ~ grp, data = d2, pooled_sd = FALSE)
# cohens_d(ch ~ grp, data = d2, pooled_sd = FALSE)
# cohens_d(mi ~ grp, data = d2, pooled_sd = FALSE)
# 
# cohens_d(sj ~ grp, data = d2, pooled_sd = FALSE)
# cohens_d(is ~ grp, data = d2, pooled_sd = FALSE)
# cohens_d(oi ~ grp, data = d2, pooled_sd = FALSE)
# 
# psych::omega(sk_df)
# psych::omega(ch_df)
# psych::omega(mi_df)
# psych::omega(sj_df)
# psych::omega(is_df)
# psych::omega(oi_df)





