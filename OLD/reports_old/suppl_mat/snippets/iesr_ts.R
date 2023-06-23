#' Compute effect size for group-comparisons on IES-R TS.

iesr_ts_df <- all_items %>% 
  dplyr::select(ies_ts, is_rescue_worker)

# Scale numeric variables.
iesr_ts_s_df <- iesr_ts_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
iesr_ts_s_df$is_rescue_worker <- relevel(iesr_ts_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "ies_ts ~ 1 + is_rescue_worker"
out_iesr_ts <- get_cohen_d(bform, b_is_rescue_workerno, iesr_ts_s_df)


# eof ---
