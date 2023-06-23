#' Userò i dati in all_items, ma filtrerò ulteriormente il campione 
#' rescue-workers. Devo selezionare da all_items il campione RW, filtrarlo
#' così
cleaned_rw_data <- temp[
  -c(9,  52, 613, 646, 1,  50,  52,  64,  79, 104, 121, 142, 198, 210, 285, 
     303, 356, 357, 401, 446, 460, 465, 468, 480, 490, 541, 545, 553, 588, 
     593, 613, 629, 639, 646, 680, 703, 741, 747, 761, 
     27,  48,  54,  58,  71, 135, 151, 162, 196, 218, 238, 263, 285, 301, 375, 
     461, 478, 592, 623, 629),
]
#' poi reinserisco i dati del campione di controllo.


















# Read the new data.
all_items <- readRDS(here("data", "processed", "data_2023", "all_items.rds"))

all_items_changed <- all_items

# Negative affect.
all_items_changed$negative_affect <- 
  abs(all_items$neoffi_1 - 4) + all_items$neoffi_11 + 
  abs(all_items$neoffi_16 - 4) + abs(all_items$neoffi_31 - 4) + 
  abs(all_items$neoffi_46 - 4)

# Self reproach.
all_items_changed$self_reproach <- 
  all_items$neoffi_6 + all_items$neoffi_21 + all_items$neoffi_26 + 
  all_items$neoffi_36 + all_items$neoffi_41 + all_items$neoffi_51 + 
  all_items$neoffi_56

# # Changes names of all_items columns.
# all_items_changed <- all_items_changed %>% 
#   rename(
#     "age_imp" = "age",
#     "interpersonal_relationships" = "relating_to_others",
#     "iperarousal" = "hyperarousal",
#     "life_appreciation" = "appreciation_of_life",
#     "spirituality_changes" = "spirituality"
#   )



# SEM ---------------------------------------------------------------------

model10 <- "
  # post-traumatic growth
  ptg =~ appreciation_of_life + new_possibilities + 
         personal_strength + spirituality + 
         relating_to_others
         
  # ptsd
  pts =~ avoiding + intrusivity + hyperarousal
  
  # coping
  cop =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # neuroticism
  neu =~ negative_affect + self_reproach

  # regressions
  ptg ~ dg_cope*cop + dg_soc*soc + dg_neuro*neu
  pts ~ ds_cope*cop + ds_soc*soc + ds_neuro*neu

  nsc ~ nsc_cope*cop + nsc_soc*soc + nsc_neuro*neu
  psc ~ psc_cope*cop + psc_soc*soc + psc_neuro*neu
  
  ptg ~ ig_nsc*nsc + ig_psc*psc 
  pts ~ is_nsc*nsc + is_psc*psc
  
  # covariances
  self_judgment ~~ self_kindness
"

temp <- all_items_changed %>% 
  dplyr::filter(group == "rescue_workers")

fit10 <- sem(
  model10,
  data = temp,
  estimator = "MLM",
  std.lv = TRUE,
  group = "group"
)

fit_meas_m10 <- fitMeasures(
  fit10, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_m10
# chisq         df        cfi cfi.robust        nfi        tli tli.robust      rmsea       srmr 
# 654.107    168.000      0.945      0.946      0.927      0.931      0.933      0.062      0.056 

# OLD
# chisq         df        cfi cfi.robust        nfi        tli tli.robust      rmsea       srmr 
# 617.996    168.000      0.946      0.948      0.928      0.933      0.935      0.061      0.058 

summary(fit10, standardized = TRUE)



# Clean the rescue-workers data.

foo <- temp[, -c(23, 24, 25, 26, 27, 28, 208:241)]
foo$id <- 1:nrow(foo)
foo$id <- factor(foo$id)

careless_long <- careless::longstring(foo)
boxplot(careless_long, main = "Boxplot of Longstring index")
which(careless_long > 23)
#  9  52 613 646

careless_long <- careless::longstring(foo, avg = T)
boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
which(careless_long$avgstr > 1.8)
# 1  50  52  64  79 104 121 142 198 210 285 303 356 357 401 446 460 465 468 480 
# 490 541 545 553 588 593 613 629 639 646 680 703 741 747 761

careless_mahad <- careless::mahad(foo[, -202])
which(careless_mahad > 330)
# 58 151 162 196 218 263 301 375 461 629

temp <- all_items_changed %>% 
  dplyr::filter(group == "rescue_workers")

cleaned_rw_data <- temp[
  -c(9,  52, 613, 646, 1,  50,  52,  64,  79, 104, 121, 142, 198, 210, 285, 
     303, 356, 357, 401, 446, 460, 465, 468, 480, 490, 541, 545, 553, 588, 
     593, 613, 629, 639, 646, 680, 703, 741, 747, 761, 
     27,  48,  54,  58,  71, 135, 151, 162, 196, 218, 238, 263, 285, 301, 375, 
     461, 478, 592, 623, 629),
]

final_sem <- cleaned_rw_data

fit10 <- sem(
  model10,
  data = final_sem,
  estimator = "MLM",
  std.lv = TRUE,
  group = "group"
)

fit_meas_m10 <- fitMeasures(
  fit10, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_m10
summary(fit10, standardized = TRUE)


# proviamo a pulire entrambi i gruppi simultaneamente

# Get only items
items_df <- d[, -c(23:28, 208:239)]

# Longstring
careless_long <- careless::longstring(items_df)
boxplot(careless_long, main = "Boxplot of Longstring index")
bad_1 <- which(careless_long > 21)

careless_long <- careless::longstring(items_df, avg = T)
boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
bad_2 <- which(careless_long$avgstr > 1.75)

careless_mahad <- careless::mahad(items_df, confidence = 0.9999)
# which(careless_mahad$flagged == TRUE)
bad_3 <- which(careless_mahad > 340)

irv_total <- careless::irv(items_df)
boxplot(irv_total, main = "Boxplot of IRV index")
bad_4 <- which(irv_total < 0.999801)

synonyms <- careless::psychsyn(items_df, .60)
boxplot(synonyms, main = "Boxplot of synonyms index")
bad_5 <- which(synonyms < 0.4329262)


rows_to_be_escluded <- unique(c(bad_1, bad_2, bad_3, bad_4, bad_5))


