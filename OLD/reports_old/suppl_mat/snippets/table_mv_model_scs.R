#' Generate table with coefficients of multivariate model with the SCS
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bform6 <- 
  bf(
    mvbind(self_kindness, self_judgment, common_humanity, isolation, 
           mindfulness, over_identification) ~ is_rescue_worker
  ) + set_rescor(FALSE)

fit6 <- brm(
  bform6, 
  family = gaussian(),
  data = all_items,
  backend = "cmdstanr"
)

# pp_check(fit6, resp = "selfkindness")
# pp_check(fit6, resp = "selfjudgment")
# pp_check(fit6, resp = "commonhumanity")
# pp_check(fit6, resp = "isolation")
# pp_check(fit6, resp = "mindfulness")
# pp_check(fit6, resp = "overidentification")

a <- summary(fit6)
summary_mod6 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars))
rownames(summary_mod6) <- c(
  "Intercept Self Kindness", "Intercept Self Judgment", 
  "Intercept Common Humanity", "Intercept Isolation", 
  "Intercept Mindfulness",  "Intercept Overidentification", 
  
  "$\\beta$ Self Kindness", "$\\beta$ Self Judgment", 
  "$\\beta$ Common Humanity", "$\\beta$ Isolation", 
  "$\\beta$ Mindfulness", "$\\beta$ Overidentification", 
  
  "$\\sigma$ Self Kindness", "$\\sigma$ Self Judgment", 
  "$\\sigma$ Common Humanity", "$\\sigma$ Isolation", 
  "$\\sigma$ Mindfulness", "$\\sigma$ Overidentification"
)

colnames(summary_mod6) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod6 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod6, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_6.csv"), 
  row.names = FALSE
)

# eof ---
