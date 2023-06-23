#' Compute effect size for group-comparisons on SCS TS.

all_items$scs_ts <- 
  all_items$pos_self_compassion + all_items$neg_self_compassion

scs_ts_df <- all_items %>% 
  dplyr::select(scs_ts, is_rescue_worker)

# Scale numeric variables.
scs_ts_s_df <- scs_ts_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
scs_ts_s_df$is_rescue_worker <- 
  relevel(scs_ts_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for comparisons between rescue workers and controls.
bform <- "scs_ts ~ 1 + is_rescue_worker"
out_scs_ts <- get_cohen_d(bform, b_is_rescue_workerno, scs_ts_s_df)


# eof ---
