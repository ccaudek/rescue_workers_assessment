#' Generate table with coefficients of multivariate model with the PTGI 
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bf_al <- bf(appreciation_of_life ~ is_rescue_worker)
bf_sp <- bf(spirituality ~ is_rescue_worker) 
bf_ps <- bf(personal_strength ~ is_rescue_worker)
bf_np <- bf(new_possibilities ~ is_rescue_worker)
bf_ro <- bf(relating_to_others ~ is_rescue_worker) 

fit3 <- brm(
  bf_al + bf_as + bf_sp + bf_np + bf_ro + set_rescor(FALSE), 
  data = all_items,
  backend = "cmdstanr",
  #family = gaussian(),
  warmup = 100, 
  iter = 300,
  cores = 6
)

fit3 <- brm(
  bf(
    mvbind(
      appreciation_of_life, spirituality, personal_strength, new_possibilities,
      relating_to_others
    ) ~ is_rescue_worker
  ) + set_rescor(FALSE), 
  data = all_items,
  backend = "cmdstanr",
  family = skew_normal(),
  # warmup = 200, 
  # iter = 1200,
  cores = 6
)

# pp_check(fit3, resp = "appreciationoflife")
# pp_check(fit3, resp = "spirituality")
# pp_check(fit3, resp = "personalstrength")
# pp_check(fit3, resp = "newpossibilities")
# pp_check(fit3, resp = "relatingtoothers")
# plot(loo(fit3))

a <- summary(fit3)
summary_mod3 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod3) <- c(
  "$\\alpha$ Appreciation of life", "$\\alpha$ Spiritual change", "$\\alpha$ Personal strength", 
  "$\\alpha$ New possibilities", "$\\alpha$ Relating to others", 
  
  "$\\beta$ Appreciation of life", "$\\beta$ Spiritual change", "$\\beta$ Personal strength", 
  "$\\beta$ New possibilities", "$\\beta$ Relating to others", 
  
  "$\\sigma_{e}$ Appreciation of life", "$\\sigma_{e}$ Spiritual change", "$\\sigma_{e}$ Personal strength", 
  "$\\sigma_{e}$ New possibilities", "$\\sigma_{e}$ Relating to others", 
  
  "$\\alpha_{e}$ Appreciation of life", "$\\alpha_{e}$ Spiritual change", "$\\alpha_{e}$ Personal strength", 
  "$\\alpha_{e}$ New possibilities", "$\\alpha_{e}$ Relating to others"
)
colnames(summary_mod3) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod3 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod3, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_3.csv"), 
  row.names = FALSE
)

# eof ---

