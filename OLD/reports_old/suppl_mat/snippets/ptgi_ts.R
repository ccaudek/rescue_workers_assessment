#' Compute effect size for group-comparisons on PTGI TS.

ptgi_ts_df <- all_items %>% 
  dplyr::select(ptgi_total_score, is_rescue_worker)

# Scale numeric variables.
ptgi_ts_s_df <- ptgi_ts_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
ptgi_ts_s_df$is_rescue_worker <- 
  relevel(ptgi_ts_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "ptgi_total_score ~ 1 + is_rescue_worker"
out_ptgi_ts <- get_cohen_d(bform, b_is_rescue_workerno, ptgi_ts_s_df)


# eof ---
