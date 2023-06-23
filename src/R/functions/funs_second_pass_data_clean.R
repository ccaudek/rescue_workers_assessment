# Generate the final data set. 
# In a first pass on the complete raw data (with IES-R corrected), I used LPA 
# computed on the careless responding measures. All the data of the three groups 
# were considered together. The groups of participants omogeneous with respect 
# too high values of the careless responding meaures were removed. Only about 
# the 3% of the data were deleted.
# Now I perform a second pass, separately for the rescue-workers group and for 
# the control group. Then, I save the cleaned data in
# here("data", "processed", "all_items", "sem_data.Rds").

second_pass_data_clean <- function(d) {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  
  # These are the data generated with a first pass from the complete raw data 
  # by using the LPA computed on the careless responding measures. Only about 
  # the 3% of the data were removed.
  # d <- readRDS(
  #   file = here(
  #     "data", "processed", "all_items", "rescue_workers_final.Rds")
  # )
  
  # Get rescue-workers sample -----------------------------------------
  
  # Find ID of bad subjects.
  
  d <- d3

  rw_df <- d %>% 
    dplyr::filter(group == "rescue_workers" & FLAG_1 == "keep")
  nrow(rw_df)
  # 746
  
  # Get only items of RW group.
  rw_items_df <- rw_df[, -c(23:28, 208:251)]
  
  
  d$sj <- NULL
  d$is <- NULL
  d$oi <- NULL
  d$family <- NULL
  d$friends <- NULL
  d$significant_other <- NULL
  d$mpss_tot <- NULL
  d$date <- NULL
  d$id <- NULL
  d$gender <- NULL
  d$age <- NULL
  d$education <- NULL
  d$employment <- NULL
  
  # Compute outliers on the careless responding measures.
  # Longstring
  careless_long <- careless::longstring(rw_items_df)
  out_1 <- boxplot(careless_long, main = "Boxplot of Longstring index")
  bad_1 <- which(careless_long > out_1$stats[5])
  
  careless_long <- careless::longstring(rw_items_df, avg = T)
  out_2 <- boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
  bad_2 <- which(careless_long$avgstr > out_2$stats[5])
  
  careless_mahad <- careless::mahad(rw_items_df, confidence = 0.999)
  # which(careless_mahad$flagged == TRUE)
  bad_3 <- which(careless_mahad > 380)
  
  irv_total <- careless::irv(rw_items_df)
  out_4 <- boxplot(irv_total, main = "Boxplot of IRV index")
  bad_4 <- which(irv_total < out_4$stats[1] | irv_total >  out_4$stats[5])
  
  synonyms <- careless::psychsyn(rw_items_df, .60)
  out_5 <- boxplot(synonyms, main = "Boxplot of synonyms index")
  bad_5 <- which(synonyms < out_5$stats[1])
  
  # Union of the rows' indices corresponding to outliers.
  rows_to_be_escluded <- unique(c(bad_1, bad_2, bad_3, bad_4, bad_5))
  # rows_to_be_escluded <- unique(c(bad_1, bad_3))
  length(rows_to_be_escluded)
  # 80
  
  # Find bad IDs for the RW group.
  rw_bad_ids <- rw_df[rows_to_be_escluded, ]$id

  # Remove observation flagged as careless responding in the RW group.
  # cleaned_rw_df <- rw_df[-rows_to_be_escluded, ]
  
  
  # Get control sample ------------------------------------------------
  
  # Repeat the same procedure in the community/students sample.
  controls_df <- d %>% 
    dplyr::filter(group != "rescue_workers")
  
  # Get only items of community/students group.
  items_controls_df <- controls_df[, -c(23:28, 208:251)]
  
  # Longstring
  careless_long <- careless::longstring(items_controls_df)
  out_1 <- boxplot(careless_long, main = "Boxplot of Longstring index")
  bad_1 <- which(careless_long > out_1$stats[5])
  
  careless_long <- careless::longstring(items_controls_df, avg = T)
  out_2 <- boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
  bad_2 <- which(careless_long$avgstr > out_2$stats[5])
  
  careless_mahad <- careless::mahad(items_controls_df, confidence = 0.9999)
  # which(careless_mahad$flagged == TRUE)
  bad_3 <- which(careless_mahad > 235 | careless_mahad < 120)
  
  irv_total <- careless::irv(items_controls_df)
  boxplot(irv_total, main = "Boxplot of IRV index")
  # bad_4 <- which(irv_total < 1.080492)
  
  synonyms <- careless::psychsyn(items_controls_df, .60)
  out_5 <- boxplot(synonyms, main = "Boxplot of synonyms index")
  bad_5 <- which(synonyms < out_5$stats[1])
  
  rows_to_be_escluded_controls <- unique(c(bad_1, bad_2, bad_3, bad_5))
  length(rows_to_be_escluded_controls)
  
  # Find bad IDs for the control group.
  cnt_bad_ids <- controls_df[rows_to_be_escluded_controls, ]$id
  
  # Remove observation flagged as careless responding in control group.
  cleaned_controls_df <- controls_df[-rows_to_be_escluded_controls, ]
  
  # Combine the two cleaned data sets ---------------------------------
  
  d$FLAG_2 <- ifelse(
    d$id %in% c(rw_bad_ids, cnt_bad_ids), "delete", "keep"
  )
  
  # 
  # new_df <- rbind(cleaned_rw_df, cleaned_controls_df)
  # 
  # # Save RDS file.
  # saveRDS(
  #   new_df,
  #   file = here(
  #     "data", "processed", "all_items", "self_comp_data.Rds")
  # )
  
  d
}


# md_second_pass_data_clean() ---------------------------------------------

# Generate the final data set. 
# In a first pass on the complete raw data (with IES-R corrected), I used LPA 
# computed on the careless responding measures. All the data of the three groups 
# were considered together. The groups of participants that were homogeneous  
# with respect to high values of the careless responding measures were removed. 
# Only about the 3% of the data were deleted. 
# The problem of second_pass_data_clean() above is that it does not change the
# fit indices, especially RMSEA from what obtained after removing the 
# participants flagged by FLAG_1. 
# In this function, I remove participants only using the Mahalanobis distance
# criterion.
# This filtering is performed separately for the rescue-workers group and for 
# the control group. 
md_second_pass_data_clean <- function(d) {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  
  # Get rescue-workers sample -----------------------------------------
  
  # Find ID of bad subjects.
  
  rw_df <- d %>% 
    dplyr::filter(group == "rescue_workers" & FLAG_1 == "keep")
  nrow(rw_df)
  # 746
  
  # Get only items of RW group.
  rw_items_df <- rw_df[, -c(23:28, 208:251)]
  
  # Compute outliers on the careless responding measures.
  # Longstring
  careless_long <- careless::longstring(rw_items_df)
  out_1 <- boxplot(careless_long, main = "Boxplot of Longstring index")
  bad_1 <- which(careless_long > out_1$stats[5])
  
  careless_long <- careless::longstring(rw_items_df, avg = T)
  out_2 <- boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
  bad_2 <- which(careless_long$avgstr > out_2$stats[5])
  
  careless_mahad <- careless::mahad(rw_items_df, confidence = 0.9999)
  # which(careless_mahad$flagged == TRUE)
  bad_3 <- which(careless_mahad > 300 | careless_mahad < 110)
  
  irv_total <- careless::irv(rw_items_df)
  out_4 <- boxplot(irv_total, main = "Boxplot of IRV index")
  bad_4 <- which(irv_total < out_4$stats[1] | irv_total >  out_4$stats[5])
  
  synonyms <- careless::psychsyn(rw_items_df, .60)
  out_5 <- boxplot(synonyms, main = "Boxplot of synonyms index")
  bad_5 <- which(synonyms < out_5$stats[1])
  
  # Union of the rows' indices corresponding to outliers.
  rows_to_be_escluded <- unique(c(bad_3))
  # rows_to_be_escluded <- unique(c(bad_1, bad_3))
  length(rows_to_be_escluded)
  # 52
  
  # Find bad IDs for the RW group.
  rw_bad_ids <- rw_df[rows_to_be_escluded, ]$id
  
  # Remove observation flagged as careless responding in the RW group.
  # cleaned_rw_df <- rw_df[-rows_to_be_escluded, ]
  
  
  # Get control sample ------------------------------------------------
  
  # Repeat the same procedure in the community/students sample.
  controls_df <- d %>% 
    dplyr::filter(group != "rescue_workers")
  
  # Get only items of community/students group.
  items_controls_df <- controls_df[, -c(23:28, 208:251)]
  
  # Longstring
  careless_long <- careless::longstring(items_controls_df)
  out_1 <- boxplot(careless_long, main = "Boxplot of Longstring index")
  bad_1 <- which(careless_long > out_1$stats[5])
  
  careless_long <- careless::longstring(items_controls_df, avg = T)
  out_2 <- boxplot(careless_long$avgstr, main = "Boxplot of Longstring index")
  bad_2 <- which(careless_long$avgstr > out_2$stats[5])
  
  careless_mahad <- careless::mahad(items_controls_df, confidence = 0.9999)
  # which(careless_mahad$flagged == TRUE)
  bad_3 <- which(careless_mahad > 235 | careless_mahad < 150)
  
  irv_total <- careless::irv(items_controls_df)
  boxplot(irv_total, main = "Boxplot of IRV index")
  # bad_4 <- which(irv_total < 1.080492)
  
  synonyms <- careless::psychsyn(items_controls_df, .60)
  out_5 <- boxplot(synonyms, main = "Boxplot of synonyms index")
  bad_5 <- which(synonyms < out_5$stats[1])
  
  rows_to_be_escluded_controls <- unique(c(bad_3))
  length(rows_to_be_escluded_controls)
  # 40
  
  # Find bad IDs for the control group.
  cnt_bad_ids <- controls_df[rows_to_be_escluded_controls, ]$id
  
  # Remove observation flagged as careless responding in control group.
  cleaned_controls_df <- controls_df[-rows_to_be_escluded_controls, ]
  
  # Combine the two cleaned data sets ---------------------------------
  
  d$FLAG_2 <- ifelse(
    d$id %in% c(rw_bad_ids, cnt_bad_ids), "delete", "keep"
  )
  
  # 
  # new_df <- rbind(cleaned_rw_df, cleaned_controls_df)
  # 
  # # Save RDS file.
  # saveRDS(
  #   new_df,
  #   file = here(
  #     "data", "processed", "all_items", "self_comp_data.Rds")
  # )
  
  d
}


