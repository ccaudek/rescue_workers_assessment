#' Generate table with coefficients of multivariate model with the NEO-FFI-60 
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bform1 <- 
  bf(mvbind(
    neuroticism, extraversion, openness, agreeableness, conscientiousness
  ) ~ is_rescue_worker) +
  set_rescor(TRUE)

fit1 <- brm(
  bform1, 
  family = gaussian(),
  data = all_items,
  backend = "cmdstanr",
  warmup = 2000, 
  iter = 12000
)

a <- summary(fit1)
summary_mod1 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod1) <- c(
  "$\\alpha$ Neuroticism", "$\\alpha$ Extraversion", "$\\alpha$ Openness", 
  "$\\alpha$ Agreeableness", "$\\alpha$ Conscientiousness", 
  "$\\beta$ Neuroticism", "$\\beta$ Extraversion", "$\\beta$ Openness", 
  "$\\beta$ Agreeableness", "$\\beta$ Conscientiousness", 
  "$\\sigma_{e}$ Neuroticism", "$\\sigma_{e}$ Extraversion", "$\\sigma_{e}$ Openness", 
  "$\\sigma_{e}$ Agreeableness", "$\\sigma_{e}$ Conscientiousness"
)
colnames(summary_mod1) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod1 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod1, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_1.csv"), 
  row.names = FALSE
)

# eof ---

