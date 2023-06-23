#' Generate table with coefficients of multivariate model with the COPE-NVI 
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bf_ss <- bf(social_support ~ is_rescue_worker)
bf_as <- bf(avoiding_strategies ~ is_rescue_worker, family = asym_laplace())
bf_pa <- bf(positive_attitude ~ is_rescue_worker)
bf_po <- bf(problem_orientation ~ is_rescue_worker)
bf_to <- bf(transcendent_orientation ~ is_rescue_worker, family = student())

fit2 <- brm(
  bf_ss + bf_as + bf_pa + bf_po + bf_to + set_rescor(FALSE), 
  data = all_items,
  backend = "cmdstanr",
  warmup = 2000, 
  iter = 12000,
  cores = 6
)

a <- summary(fit2)

summary_mod2 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars))

rownames(summary_mod2) <- c(
  "$\\alpha$ Social support", "$\\alpha$ Avoiding strategies", 
  "$\\alpha$ Positive attitude", "$\\alpha$ Problem orientation", 
  "$\\alpha$ Trascendental orientation", 
  
  "$\\beta$ Social support", "$\\beta$ Avoiding strategies", 
  "$\\beta$ Positive attitude", "$\\beta$ Problem orientation", 
  "$\\beta$ Trascendental orientation", 
  
  "$\\sigma_{e}$ Social support", "$\\sigma_{e}$ Avoiding strategies", 
  "$\\sigma_{e}$ Positive attitude", 
  "$\\sigma_{e}$ Problem orientation", "$\\sigma_{e}$ Trascendental orientation",
  "$q$ Avoiding strategies", "$\\nu$ Trascendental orientation"
)

colnames(summary_mod2) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod2 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod2, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_2.csv"), 
  row.names = FALSE
)

# eof ---
