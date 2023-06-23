#' Compute effect size for group-comparisons on each COPE-NVI subscale.

# Select COPE-NVI subscales and group.

ptgi_df <- all_items %>% 
  dplyr::select(
    relating_to_others, new_possibilities, personal_strength, 
    appreciation_of_life, spirituality, is_rescue_worker
  )

# Scale numeric variables.
ptgi_s_df <- ptgi_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
ptgi_s_df$is_rescue_worker <- relevel(ptgi_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "relating_to_others ~ 1 + is_rescue_worker"
out_ro <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_s_df)

bform <- "new_possibilities ~ 1 + is_rescue_worker"
out_np <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_s_df)

bform <- "personal_strength ~ 1 + is_rescue_worker"
out_ps <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_s_df)

bform <- "appreciation_of_life ~ 1 + is_rescue_worker"
out_al <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_s_df)

bform <- "spirituality ~ 1 + is_rescue_worker"
out_sc <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_s_df)

# eof ---
