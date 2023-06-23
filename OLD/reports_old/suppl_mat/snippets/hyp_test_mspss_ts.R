#' Compute probability of the group difference on MSPSS TS.

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

h_mspss_ts <- brm(
  bform,
  data = mspss_ts_s_df,
  family = skew_normal(),
  backend = "cmdstanr"
)

pp_check(h_mspss_ts)
summary(h_mspss_ts)

out_h_mspss_ts <- hypothesis(h_mspss_ts, 'is_rescue_workerno < 0')

res_h_mspss_ts <- c(out_h_mspss_ts$hypothesis[6], out_h_mspss_ts$hypothesis[7])


# eof ---
