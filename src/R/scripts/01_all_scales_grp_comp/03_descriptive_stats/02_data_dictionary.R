# Script name: 02_data_dictionary.R
# Project: project
# Script purpose: script_purpose
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: last_modified_date
# 
# Notes: 

library("here")
library("tidyverse")

d <- rio::import(
  here::here("data", "processed", "self_compassion_total.csv")
)

d$grp <- ifelse(d$group == "rescue_workers", "rescue_worker", "community_sample")
d$group <- NULL

d$date <- NULL
d$education <- NULL
d$employment <- NULL
d$is_rescue_worker <- NULL
d$red_cross_commeetee_location <- NULL
d$rescue_worker_qualification <- NULL
d$last_training <- NULL
d$rate_of_activity <- NULL
d$is_job_qualification_invariant <- NULL
d$is_team_invariant <- NULL
d$is_married <- NULL
d$FLAG_2 <- NULL
d$pos_sc <- NULL
d$neg_sc <- NULL

d <- d |> 
  dplyr::rename(
    "rate_of_activity" = "rate_of_activity_num",
    "last_training" = "last_training_num",
    "education" = "education_num",
    "FLAG" = "FLAG_1"
  )

# Add random code for each row.
d$id <- stringi::stri_rand_strings(nrow(d), 7)
nrow(d)
length(unique(d$id))

# Remove scales COPE not used.
d1 <- d %>% 
  dplyr::select(
    -c(
      cope_4  , cope_14 , cope_30 , cope_45 ,
      cope_11 , cope_23 , cope_34 , cope_52 ,
      cope_3  , cope_17 , cope_28 , cope_46 ,
      cope_6  , cope_27 , cope_40 , cope_57 ,
      cope_9  , cope_24 , cope_37 , cope_51 ,
      cope_2  , cope_16 , cope_31 , cope_43 ,
      cope_12 , cope_26 , cope_35 , cope_53 ,
      cope_8  , cope_20 , cope_36 , cope_50 , 
      cope_7  , cope_18 ,cope_48 , cope_60,
      cope_total_score
    )
  )

# Remove SCS subscales.

d1 <- d1 %>% 
  dplyr::select(
    -c(
      self_kindness, common_humanity, isolation, 
      self_judgment, mindfulness, over_identification
    )
  )


# Remove total scores of all scales.
d1 <- d1 %>% 
  dplyr::select(
    -c(
      ies_ts, ptgi_total_score, social_support, avoiding_strategies,
      transcendent_orientation, family, friends, significant_other,
      mpss_tot, ts_sc, neg_self_compassion, pos_self_compassion
    )
  )

d1 <- d1 %>% 
  dplyr::select(
    -starts_with("mspss_")
  )

d1$education <- ifelse(d1$education == 22, 21, d1$education)

d1 %>% 
  group_by(grp) %>% 
  summarize(
    edu = mean(education),
    edu_sd = sd(education)
  )

d2 <- d1 %>% 
  dplyr::select(
    -c()
  )

write.csv(
  d1,
  here::here("data", "processed", "self_compassion.csv"), row.names = FALSE)

  
  
  




