# Script name: 02_compute_subscales.R
# Project: Self compassion: one construct or two?
# Script purpose: Joining the separate data sets of the three samples. 
#   In this script, only the sub-scales of each questionnaire are used. The 
#   data does not include the individual items. So, on these data is not 
#   possible to perform the careless responding analysis.
#   The purpose is the comparison of the rescue-workers sample and
#   the control group, so as to highlight the specificity of the
#   rescue-workers sample. The comparison is made according to
#   the six scales that had been administered.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Feb  4 10:55:28 2020
# Last Modified Date: Sat Sep 17 10:55:12 2022
# 
# Notes: In the self-compassion project, the following scales have been 
# administered.
#
# - NEO FFI 60: neuroticism, extraversion, openness, agreeableness, 
#               conscientiousness 
# 
# - COPE: social_support, positive_attitude, problem_orientation, 
#         transcendent_orientation
# 
# - PTG: interpersonal_relationships, new_possibilities, personal_strength, 
#        life_appreciation, spirituality_changes 
#
# - IES-R: avoiding, intrusivity, iperarousal (components of PTSD)
# 
# - all_items: self_kindness, self_judgment, common_humanity, isolation, mindfulness, 
#        over_identification
#        neg_self_compassion <- self_judgment + isolation + over_identification
#        pos_self_compassion <- self_kindness + common_humanity + mindfulness
# 
# - all_items: family, friends, significant_other 


# Set up ----

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
library("mice")


source(here("src", "R", "functions", "funs_flag_careless_responding.R"))
source(here("src", "R", "functions", "funs_compute_subscales_scores.R"))
source(here("src", "R", "functions", "funs_correct_iesr_scores.R"))
source(here("src", "R", "functions", "funs_second_pass_data_clean.R"))

options(max.print=999999)

set.seed(12345)


# Read complete data. These data must be cleaned.
d1 <- readRDS(
  file = here(
    "data", "processed", "all_items",
    "final_complete_rescue_workers_controls_data.Rds"
  )
)


# Flag careless responding with LPA ---------------------------------------

d2 <- flag_careless_responding_LPA(d1)

# Add subscales scores ----------------------------------------------------

# In the self_judgment, over_identification and isolation variable
# the data have been reversed-scored (they mean *absence* of  SJ, OI, IS) to 
# compute the scs_ts.
# The sj, oi, is variables are not reversed-scored: they mean presence of 
# self-judgment, over-identification, and isolation.
d3 <- compute_subscales_scores(d2)

cor(cbind(
  d3$sk, d3$ch, d3$mi, 
  d3$sj, d3$oi, d3$is,
  d3$scs_ts
)
) |> 
  round(2)


# saveRDS(
#   all_items, 
#   file = here("data", "processed", "all_items", 
#               "three_samples_all_items_and_subscales.Rds")
# )


# Correct IES-R scores ----------------------------------------------------

# In the original data, the values of the IES-R items for the two control groups
# are too high: Half of the sample has values > 34, that is, they are of  
# clinical interest. These same participants have 'normal' values on the other 
# scales. So, I decided to 'shift' the score distributions of the control groups 
# so as to make them less extreme. 
# The transformed data set is saved in 
# here("data", "processed", "all_items", "three_samples_items_final.Rds").
d4 <- correct_iesr_scores(d3)
d4$red_cross_commeetee_location <- d3$red_cross_commeetee_location

# saveRDS(
#   all_items_final,
#   file = here(
#     "data", "processed", "all_items", "three_samples_items_final.Rds")
# )


# Second-pass to flag careless responding  --------------------------------

# Change this if you want to preserve more subjects.
# d5 <- md_second_pass_data_clean(d4)

saveRDS(
  d4,
  file = here(
    "data", "processed", "all_items", "rescue_workers_final.Rds")
)


# eof ---

