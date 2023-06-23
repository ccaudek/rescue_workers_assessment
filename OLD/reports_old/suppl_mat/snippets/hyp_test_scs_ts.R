#' Compute probability of the group difference on SCS TS.

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

# Group difference between rescue workers and controls.
bform <- "scs_ts ~ 1 + is_rescue_worker"
h_scs_ts <- brm(
  bform,
  data = scs_ts_s_df,
  family = gaussian(),
  backend = "cmdstanr"
)
# pp_check(h_scs_ts)
# summary(h_scs_ts)

out_h_scs_ts <- hypothesis(h_scs_ts, 'is_rescue_workerno < 0')
res_h_scs_ts <- c(out_h_scs_ts$hypothesis[6], out_h_scs_ts$hypothesis[7])

# Group comparison on positive SC.

scs_pos_neg_df <- all_items %>% 
  dplyr::select(pos_self_compassion, neg_self_compassion, is_rescue_worker)

# Scale numeric variables.
scs_pos_neg_s_df <- scs_pos_neg_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

bform <- "pos_self_compassion ~ 1 + is_rescue_worker"
h_scs_pos <- brm(
  bform,
  data = scs_pos_neg_s_df,
  family = gaussian(),
  backend = "cmdstanr"
)

# pp_check(h_scs_pos)
# summary(h_scs_pos)

out_h_scs_pos <- hypothesis(h_scs_pos, 'is_rescue_workerno < 0')
res_h_scs_pos <- c(out_h_scs_pos$hypothesis[6], out_h_scs_pos$hypothesis[7])

# Group comparison on negative SC.

bform <- "neg_self_compassion ~ 1 + is_rescue_worker"
h_scs_neg <- brm(
  bform,
  data = scs_pos_neg_s_df,
  family = gaussian(),
  backend = "cmdstanr"
)
# pp_check(h_scs_neg)
# summary(h_scs_neg)

out_h_scs_neg <- hypothesis(h_scs_neg, 'is_rescue_workerno < 0')
res_h_scs_neg <- c(out_h_scs_neg$hypothesis[6], out_h_scs_neg$hypothesis[7])

# Model comparison.
bform <- "neg_self_compassion ~ 1"
h_scs_neg_null <- brm(
  bform,
  data = scs_pos_neg_s_df,
  family = gaussian(),
  backend = "cmdstanr"
)

# fit1 <- add_criterion(h_scs_neg_null, "loo")
# fit2 <- add_criterion(h_scs_neg, "loo")
# loo_compare(fit1, fit2, criterion = "loo")
#    elpd_diff se_diff
# fit2   0.0       0.0  
# fit1 -37.4       9.2 

# eof ---
