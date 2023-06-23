#' Compute effect size for group-comparisons on each SCS subscale.

# Select SCS subscales and group.
scs_df <- all_items %>% 
  dplyr::select(
    self_kindness, self_judgment, common_humanity, isolation, 
    mindfulness, over_identification, 
    neg_self_compassion, pos_self_compassion,
    is_rescue_worker
  )

# Scale numeric variables.
scs_s_df <- scs_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
scs_s_df$is_rescue_worker <- relevel(scs_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for rescue-workers/controls comparisons. 
bform <- "self_kindness ~ 1 + is_rescue_worker"
out_sk <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

bform <- "self_judgment ~ 1 + is_rescue_worker"
out_sj <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

bform <- "common_humanity ~ 1 + is_rescue_worker"
out_ch <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

bform <- "isolation ~ 1 + is_rescue_worker"
out_is <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

bform <- "mindfulness ~ 1 + is_rescue_worker"
out_mi <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

bform <- "over_identification ~ 1 + is_rescue_worker"
out_oi <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)

# Effect size for SCS Negative. 
bform <- "neg_self_compassion ~ 1 + is_rescue_worker"
out_cohen_sc_neg <- get_cohen_d(bform, b_is_rescue_workerno, scs_s_df)


# eof ---
