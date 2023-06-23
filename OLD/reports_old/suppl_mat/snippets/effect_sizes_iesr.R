#' Compute effect size for group-comparisons on each IES-R subscale.

# Select IES-R subscales and group.
iesr_df <- all_items %>% 
  dplyr::select(avoiding, intrusivity, hyperarousal, is_rescue_worker)

# Scale numeric variables.
iesr_s_df <- iesr_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

# Relevel groups factor.
iesr_s_df$is_rescue_worker <- relevel(iesr_s_df$is_rescue_worker, ref = "yes")

# Compute effect size for rescue-workers/controls comparisons. 
bform <- "avoiding ~ 1 + is_rescue_worker"
out_avoiding <- get_cohen_d(bform, b_is_rescue_workerno, iesr_s_df)

bform <- "intrusivity ~ 1 + is_rescue_worker"
out_intrusivity <- get_cohen_d(bform, b_is_rescue_workerno, iesr_s_df)

bform <- "hyperarousal ~ 1 + is_rescue_worker"
out_hyperarousal <- get_cohen_d(bform, b_is_rescue_workerno, iesr_s_df)

# eof ---



