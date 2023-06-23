# Script name: 10_run_mplus_scs_groups.R
# Project: rescue-workers
# Script purpose: SCS validation: SCS validation for two groups.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Oct 10 13:26:07 2022
# Last Modified Date: Mon Oct 24 05:43:40 2022
# 
# Notes: 


library("here")
library("tidyverse")
library("MplusAutomation")
library("gt")
library("glue")
library("kableExtra")
library("misty")

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6 # Use 6 cores
)

source(
  here::here(
    "src", "R", "functions", "funs_add_neoffi60_subscales.R"
  )
)

# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here(
  "src", "R", "scripts", "02_mplus", "mplus_models", "scs_group_diff"
)


# Save data for Mplus -----------------------------------------------------

# Read the complete data.
d <- readRDS(
  file = here("data", "processed", "all_items", "rescue_workers_final.Rds")
)
nrow(d)

# Add NEO-FFI-60 subscales.
d1 <- add_neoffi60_subscales(d)

d2 <- d1 |> 
  dplyr::filter(FLAG_1 == "keep" & FLAG_2 == "keep") # & FLAG_2 == "keep"

d2 |> 
  group_by(group) |> 
  summarise(
    n = n()
  )

# d2$grp <- ifelse(
#   d2$group == "rescue_workers", "RW", 
#   ifelse(d2$group != "rescue_workers" & d2$employment == "Studente", "ST", "CS")
# )

d2$grp <- ifelse(d2$group == "rescue_workers", 0, 1)

d2 |> 
  group_by(grp) |> 
  summarise(
    n = n()
  )

# d3 <- d2 |> 
#   dplyr::filter((group == "rescue_workers") | (group != "rescue_workers" & FLAG_2 == "keep")) 

d3 <- d2 |> 
  dplyr::filter(group == "rescue_workers") 

d3 <- d2 |>
  dplyr::filter(
    (group == "rescue_workers") | 
    (group == "student_sample") | 
    (group == "community_sample") # (group == "community_sample" & FLAG_2 == "keep") 
    )

# d3 <- d2 |> 
#   dplyr::filter((group == "rescue_workers") | (group == "student_sample")) 


# # Add grp.
# d3$grp <- ifelse(d3$group == "rescue_workers", 1,
#                  ifelse(d3$group == "student_sample", 2, 3)) 

comm_sample <- d2 |> 
  dplyr::filter(group == "student_sample")





#-------------

comm_sample <- comm_sample |> 
  dplyr::filter(
    # employment == "Pensionata" |
    #   employment == "Pensionato" |
      employment == "Disoccupato" |
      # employment != "Studente" &
      employment == "Impiegato" |
      # employment != "Libero professionista" &
      employment == "Operaio" 
      # employment != "Casalinga"
      ) 

#-------------


# Use data of all groups!
sem_df <- d2 |> 
  dplyr::select(
    # SCS
    all_of(contains("scs_")), 
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
    "src", "R", "scripts", "02_mplus", "mplus_models", "scs_group_diff", "semdatagr.dat"
  )
)


# Run Mplus models --------------------------------------------------------


# Model 4b Bifactor-ESEM (1 G- and 6 S-factors) ---------------------------

# Free loading-matrix by group.
runModels(paste0(the_dir, "/gm4b.inp"), showOutput = TRUE)
gm4b <- MplusAutomation::readModels(paste0(the_dir, "/gm4b.out"))
summary(gm4b)
# Model: Chi2(df = 480) = 777.263, p = 0 
# Baseline model: Chi2(df = 650) = 11793.301, p = 0 
# CFI = 0.973, TLI = 0.964, SRMR = 0.032 
# RMSEA = 0.034, 90% CI [0.03, 0.038], p < .05 = 1 
# AIC = 74955.11, BIC = 76317.861 

# A single group
runModels(paste0(the_dir, "/m4b.inp"), showOutput = TRUE)
m4b <- MplusAutomation::readModels(paste0(the_dir, "/m4b.out"))
summary(m4b)
# Model: Chi2(df = 164) = 408.534, p = 0 
# Baseline model: Chi2(df = 325) = 11627.737, p = 0 
# CFI = 0.978, TLI = 0.957, SRMR = 0.014 
# RMSEA = 0.037, 90% CI [0.033, 0.042], p < .05 = 1 
# AIC = 75172.84, BIC = 76232.205 

# Single group
runModels(paste0(the_dir, "/m5b.inp"), showOutput = TRUE)
m5b <- MplusAutomation::readModels(paste0(the_dir, "/m5b.out"))
summary(m5b)
# Model: Chi2(df = 157) = 511.151, p = 0 
# Baseline model: Chi2(df = 325) = 11627.737, p = 0 
# CFI = 0.969, TLI = 0.935, SRMR = 0.014 
# RMSEA = 0.046, 90% CI [0.042, 0.05], p < .05 = 0.931 
# AIC = 75125.252, BIC = 76219.431 

# Two groups
runModels(paste0(the_dir, "/gm6b.inp"), showOutput = TRUE)
gm6b <- MplusAutomation::readModels(paste0(the_dir, "/gm6b.out"))
summary(gm6b)
# Model: Chi2(df = 584) = 1337.067, p = 0 
# Baseline model: Chi2(df = 650) = 11793.301, p = 0 
# CFI = 0.932, TLI = 0.925, SRMR = 0.048 
# RMSEA = 0.049, 90% CI [0.046, 0.053], p < .05 = 0.653 
# AIC = 75433.198, BIC = 76278.701 

# Single group
runModels(paste0(the_dir, "/m6b.inp"), showOutput = TRUE)
m6b <- MplusAutomation::readModels(paste0(the_dir, "/m6b.out"))
summary(m6b)
# Model: Chi2(df = 246) = 888.044, p = 0 
# Baseline model: Chi2(df = 325) = 11627.737, p = 0 
# CFI = 0.943, TLI = 0.925, SRMR = 0.029 
# RMSEA = 0.049, 90% CI [0.046, 0.053], p < .05 = 0.598 
# AIC = 75637.479, BIC = 76289.014 




# Single group
runModels(paste0(the_dir, "/sem_6b.inp"), showOutput = TRUE)
sem_6b <- MplusAutomation::readModels(paste0(the_dir, "/sem_6b.out"))
summary(sem_6b)






# Group loading-matrix invariance.
runModels(paste0(the_dir, "/gm4b_2.inp"), showOutput = TRUE)
gm4b_2 <- MplusAutomation::readModels(paste0(the_dir, "/gm4b_2.out"))
summary(gm4b_2)
# Model: Chi2(df = 465) = 829.898, p = 0 
# CFI = 0.967, TLI = 0.954, SRMR = 0.05 
# RMSEA = 0.038, 90% CI [0.034, 0.043], p < .05 = 1 
# AIC = 75039.915, BIC = 76477.269 

1 - pchisq(952.011 - 829.898, 487 - 465)
# [1] 6.661338e-16
# Evidence of different loadings in the two groups.


# Model 5b ----------------------------------------------------------------

# Free loading-matrix by group.
runModels(paste0(the_dir, "/gm5b_1.inp"), showOutput = TRUE)
gm5b_1 <- MplusAutomation::readModels(paste0(the_dir, "/gm5b_1.out"))
summary(gm5b_1)
# Model: Chi2(df = 478) = 721.196, p = 0 
# CFI = 0.978, TLI = 0.97, SRMR = 0.03 
# RMSEA = 0.031, 90% CI [0.026, 0.035], p < .05 = 1 
# AIC = 74885.524, BIC = 76258.222 

# Group loading-matrix invariance.
runModels(paste0(the_dir, "/gm5b_2.inp"), showOutput = TRUE)
gm5b_2 <- MplusAutomation::readModels(paste0(the_dir, "/gm5b_2.out"))
summary(gm5b_2)



# Model 6b: Bifactor ESEM (1 G- and 2 S-factors) --------------------------

# Free loading-matrix by group.
runModels(paste0(the_dir, "/gm6b_1.inp"), showOutput = TRUE)
gm6b_1 <- MplusAutomation::readModels(paste0(the_dir, "/gm6b_1.out"))
summary(gm6b_1)
# Model: Chi2(df = 565) = 1368.255, p = 0 
# CFI = 0.928, TLI = 0.917, SRMR = 0.061 
# RMSEA = 0.052, 90% CI [0.048, 0.055], p < .05 = 0.222 
# AIC = 75511.037, BIC = 76451.036 

# Fixed loading-matrix by group.
runModels(paste0(the_dir, "/gm6b_2.inp"), showOutput = TRUE)
gm6b_2 <- MplusAutomation::readModels(paste0(the_dir, "/gm6b_2.out"))
summary(gm6b_2)
# Model: Chi2(df = 587) = 1486.32, p = 0 
# CFI = 0.919, TLI = 0.911, SRMR = 0.074 
# RMSEA = 0.054, 90% CI [0.05, 0.057], p < .05 = 0.041 
# AIC = 75597.196, BIC = 76427.777 

1 - pchisq(1486.32 - 1368.255, 587 - 565)
# [1] 3.885781e-15


fs <- read.table(
  paste0(the_dir, "/m6b_scores.txt")
)


item_scores <- fs[, 1:26]

# scsk5 scsk12 scsk19 scsk23 scsk26
# scsj1 scsj8 scsj11 scsj16 scsj21
# scch3 scch7 scch10 scch15
# scis4 scis13 scis18 scis25
# scmi9 scmi14 scmi17 scmi22
# scoi2 scoi6 scoi20 scoi24

pos_item_scores <- item_scores[
  , c(
    5, 12, 19, 23, 26,
    3, 7, 10, 15,
    9, 14, 17, 22
    )]
pos_ts <- rowSums(pos_item_scores)

neg_item_scores <- item_scores[
  , c(
    1, 8, 11, 16, 21,
    4, 13, 18, 25,
    2, 6, 20, 24
  )]
neg_ts <- rowSums(neg_item_scores)

plot(fs$V29, pos_ts)
cor(fs$V29, pos_ts)
# [1] 0.8282258

plot(fs$V31, neg_ts)
cor(fs$V31, neg_ts)

plot(fs$V27, neg_ts)
cor(fs$V27, neg_ts)
# [1] 0.8102577

scs_ts <- rowSums(item_scores)
cor(scs_ts, fs$V31)
# [1] 0.7138899

