#' Generate table with coefficients of multivariate model with the MSPSS
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bform5 <- 
  bf(
    mvbind(family, friends, significant_other) ~ is_rescue_worker
  ) + set_rescor(FALSE)

fit5 <- brm(
  bform5, 
  family = asym_laplace(),
  data = all_items,
  backend = "cmdstanr"
  # algorithm = "meanfield"
)

# pp_check(fit5, resp = "family")
# pp_check(fit5, resp = "friends")
# pp_check(fit5, resp = "significantother")

a <- summary(fit5)
summary_mod5 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod5) <- c(
  "Intercept Family", "Intercept Friends", "Intercept Significant Other", 
  "$\\beta$ Family", "$\\beta$ Friends", "$\\beta$ Significant Other", 
  "$\\sigma$ Family", "$\\sigma$ Friends", "$\\sigma$ Significant Other", 
  "$q$ Family", "$q$ Friends", "$q$ Significant Other"
)
colnames(summary_mod5) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod5 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod5, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_5.csv"), 
  row.names = FALSE
)


# eof ---

