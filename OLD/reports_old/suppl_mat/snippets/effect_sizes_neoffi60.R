#' Compute effect size for group-comparisons on each NEO-FFI-60 subscale.

# Select NEO-FFI-60 subscales and group.
neoffi_df <- all_items %>% 
  dplyr::select(neuroticism, extraversion, openness, agreeableness, 
                conscientiousness, is_rescue_worker)

# Scale numeric variables.
neoffi_s_df <- neoffi_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
neoffi_s_df$is_rescue_worker <- relevel(neoffi_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for rescue-workers/controls comparisons. 
bform <- "neuroticism ~ 1 + is_rescue_worker"
out_neuro <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)

bform <- "extraversion ~ 1 + is_rescue_worker"
out_extra <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)

bform <- "openness ~ 1 + is_rescue_worker"
out_open <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)

bform <- "agreeableness ~ 1 + is_rescue_worker"
out_agre <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)

bform <- "conscientiousness ~ 1 + is_rescue_worker"
out_cons <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)


