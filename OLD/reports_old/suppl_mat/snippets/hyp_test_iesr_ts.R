#' Compute probability of the group difference on IES-R TS.

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

h_iesr_ts <- brm(
  bform,
  data = iesr_ts_s_df,
  family = skew_normal(),
  backend = "cmdstanr"
)

pp_check(h_iesr_ts)
summary(h_iesr_ts)

out_h_iesr_ts <- hypothesis(h_iesr_ts, 'is_rescue_workerno > 0')

res_h_iesr_ts <- c(out_h_iesr_ts$hypothesis[6], out_h_iesr_ts$hypothesis[7])


# eof ---
