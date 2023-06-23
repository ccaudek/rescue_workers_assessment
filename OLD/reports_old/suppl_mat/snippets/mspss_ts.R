#' Compute effect size for group-comparisons on MSPSS TS.

mspss_ts_df <- all_items %>% 
  dplyr::select(mpss_tot, is_rescue_worker)

# Scale numeric variables.
mspss_ts_s_df <- mspss_ts_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
mspss_ts_s_df$is_rescue_worker <- relevel(mspss_ts_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "mpss_tot ~ 1 + is_rescue_worker"
out_mspss_ts <- get_cohen_d(bform, b_is_rescue_workerno, mspss_ts_s_df)

# eof ---
