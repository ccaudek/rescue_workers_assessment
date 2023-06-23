#' In the original data, the values of the IES-R items for the two control groups
#' are too high: Half of the sample has values > 34, that is, of clinical 
#' interest. These same participants have 'normal' values on the other scales.
#' I don't know why they provided such answers to the IES-R questionnaire. I
#' decided to 'shift' the score distributions of the control groups so as to make
#' less extreme. To do so, (1) I extract the `ies_` items from here("data", 
#' "processed", "all_items", "three_samples_all_items_and_subscales.Rds"), 
#' together with "avoiding", "intrusivity", "hyperarousal", "ies_total_score", 
#' only for the two control groups, (2) change the scores, and (3) put them
#' back in the complete data set. The transformed data set is saved in 
#' here("data", "processed", "all_items", "three_samples_items_final.Rds").

correct_iesr_scores <- function(all_items) {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  library("mice")
 
  options(max.print=999999)
  
  # # Read data 
  # all_items <- readRDS(
  #   file = here("data", "processed", "all_items", 
  #               "three_samples_all_items_and_subscales.Rds")
  # )
  
  all_items$ies_ts <- NULL
  
  # Correction of IES-R items
  
  # Step 1.
  
  # Add participant's numeric identifier.
  all_items$idx <- 1:nrow(all_items)
  
  dim(all_items)
  # [1] 1120  250
  
  # All columns without IES-R items and its subscales.
  all_items_without_ies <- all_items %>% 
    dplyr::select(
      !c(num_range("ies_", 1:22), avoiding, intrusivity, hyperarousal)
    )
  dim(all_items_without_ies)
  # [1] 1120  225
  
  # Only IES-R items, its subscales, id, and group.
  all_items_ies <- all_items %>% 
    dplyr::select(
      c(num_range("ies_", 1:22), idx, id, group)
    )
  dim(all_items_ies)
  # [1] 1120   24
  
  # Split all_items_ies in ies_rescue_workers vs. ies_controls.
  ies_rescue_workers <- all_items_ies %>% 
    dplyr::filter(group == "rescue_workers")
  nrow(ies_rescue_workers)
  # [1] 783
  
  ies_controls <- all_items_ies %>% 
    dplyr::filter(group != "rescue_workers")
  nrow(ies_controls)
  # [1] 337
  
  nrow(ies_rescue_workers) + nrow(ies_controls) == nrow(all_items)
  # [1] TRUE
  
  # Step 2.
  
  # Change ies_controls. 
  
  # Get 90% of the rows of the control sample.
  set.seed(12345)
  sample_of_temp1 <- sample_n(ies_controls, round(nrow(ies_controls) * 0.5))  
  # Find ids of the selected 90%.
  indices_of_subset_big <- sample_of_temp1$idx
  length(indices_of_subset_big)
  # [1] 303
  
  # Find ids of the remaining 10%.
  indices_of_subset_small <- setdiff(ies_controls$idx, indices_of_subset_big)
  length(indices_of_subset_small)
  # [1] 34
  
  # Check.
  length(indices_of_subset_big) + length(indices_of_subset_small) == 
    length(ies_controls$id)
  
  # Of the selected 90%, shrinking the item score distributions towards the 0.
  temp2 <- sample_of_temp1 %>% 
    mutate(across(starts_with("ies_"), ~ scales::rescale(.x, c(0, 1.0)))) 
  # Round the resuls
  temp3 <- temp2 %>% 
    mutate(across(starts_with("ies_"), ~ round(.))) 
  nrow(temp3)
  # [1] 303
  
  # Both groups: old IESR items for the RW group; changed IESR items for the CS
  # group.
  new_ies <- rbind(
    ies_rescue_workers, 
    temp3,
    all_items_ies[indices_of_subset_small, ]
  )
  dim(new_ies)
  # [1] 1120   24
  # table(new_ies$id)
  
  # Create IES-R subscales from the changed data.
  new_ies$avoiding <- NULL
  new_ies$intrusivity <- NULL
  new_ies$hyperarousal <- NULL
  
  new_ies$avoiding <- with(
    new_ies,
    ies_5 + ies_7 + ies_8 + ies_11 + ies_12 + ies_13 + ies_17 + ies_22
  )
  
  new_ies$intrusivity <- with(
    new_ies,
    ies_1 + ies_2 + ies_3 + ies_6 + ies_9 + ies_14 + ies_16 + ies_20
  )
  
  new_ies$hyperarousal <- with(
    new_ies,
    ies_4 + ies_10 + ies_15 + ies_18 + ies_19 + ies_21
  )

  # new_ies %>%
  #   group_by(group) %>%
  #   summarise(
  #     a = mean(avoiding),
  #     i = mean(intrusivity),
  #     h = mean(hyperarousal)
  #   )
  
  # new_subscales <- cbind(
  #   a = new_ies$avoiding, i = new_ies$intrusivity, h = new_ies$hyperarousal
  # )
  # cor(new_subscales)
  # 
  # old_subscales <- cbind(
  #   a = all_items$avoiding, i = all_items$intrusivity, h = all_items$hyperarousal
  # )
  # cor(old_subscales)
  
  # Compute IES TS
  ies_items_final <- new_ies %>%
    mutate(ies_ts = rowSums(across(starts_with("ies_"))))
  
  ies_items_final$grp <- ifelse(ies_items_final$group == "rescue_workers", 1, 0)

  ies_items_final %>%
    group_by(grp) %>%
    summarize(
      avg_ies_ts = mean(ies_ts)
    )
  
  # m <- brm(
  #   avoiding ~ grp, 
  #   family = skew_normal(),
  #   data = ies_items_final,
  #   
  # )
  
  ies_items_final$idx <- NULL
  all_items_without_ies$idx <- NULL
  
  all_items_final <- full_join(
    ies_items_final, all_items_without_ies, by = c("id", "group")
  )
  dim(all_items_final)
  
  all_items_final$iesr_ts <- with(
    all_items_final,
    ies_1 + ies_2 + ies_3 + ies_4 + ies_5 + ies_6 + ies_7 + ies_8 + ies_9 + 
      ies_10 + ies_11 + ies_12 + ies_13 + ies_14 + ies_15 + ies_16 + ies_17 + 
      ies_18 + ies_19 + ies_20 + ies_21 + ies_22 
  )
  
  all_items_final
}
