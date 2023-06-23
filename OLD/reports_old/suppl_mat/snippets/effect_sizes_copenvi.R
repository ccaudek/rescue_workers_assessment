#' Compute effect size for group-comparisons on each COPE-NVI subscale.

# Select COPE-NVI subscales and group.

cope_df <- all_items %>% 
  dplyr::select(
    social_support, avoiding_strategies, positive_attitude, problem_orientation,
    transcendent_orientation, is_rescue_worker
  )

# Scale numeric variables.
cope_s_df <- cope_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
cope_s_df$is_rescue_worker <- relevel(cope_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "social_support ~ 1 + is_rescue_worker"
out_ss <- get_cohen_d(bform, b_is_rescue_workerno, cope_s_df)

bform <- "avoiding_strategies ~ 1 + is_rescue_worker"
out_as <- get_cohen_d(bform, b_is_rescue_workerno, cope_s_df)

bform <- "positive_attitude ~ 1 + is_rescue_worker"
out_pa <- get_cohen_d(bform, b_is_rescue_workerno, cope_s_df)

bform <- "problem_orientation ~ 1 + is_rescue_worker"
out_po <- get_cohen_d(bform, b_is_rescue_workerno, cope_s_df)

bform <- "transcendent_orientation ~ 1 + is_rescue_worker"
out_to <- get_cohen_d(bform, b_is_rescue_workerno, cope_s_df)

# eof ---
