#' Computes the probability of the group difference on the TS.

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

h_ptgi_ts <- brm(
  bform,
  data = ptgi_ts_s_df,
  family = skew_normal(),
  backend = "cmdstanr"
)

pp_check(h_ptgi_ts)
summary(h_ptgi_ts)

out_h_ptgi_ts <- hypothesis(h_ptgi_ts, 'is_rescue_workerno > 0')

res_h_ptgi_ts <- c(out_h_ptgi_ts$hypothesis[6], out_h_ptgi_ts$hypothesis[7])


# eof ---
