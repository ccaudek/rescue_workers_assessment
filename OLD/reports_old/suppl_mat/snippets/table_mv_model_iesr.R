#' Generate table with coefficients of multivariate model with the IES-R 
#' subscales as DVs and group (rescue-workers/controls) as predictor.

# Multivariate model.
bform4 <- 
  bf(
    mvbind(avoiding, intrusivity, hyperarousal) ~ is_rescue_worker
  ) + set_rescor(FALSE)

fit4 <- brm(
  bform4, 
  family = skew_normal(),
  data = all_items,
  backend = "cmdstanr",
  warmup = 200, 
  iter = 1200
)

# pp_check(fit4, resp = "avoiding")
# pp_check(fit4, resp = "intrusivity")
# pp_check(fit4, resp = "hyperarousal")

a <- summary(fit4)
summary_mod4 <- rbind(data.frame(a$fixed), data.frame(a$spec_pars) )
rownames(summary_mod4) <- c(
  "Intercept Avoiding", "Intercept Intrusivity", "Intercept Hyperarousal", 
  "$\\beta$ Avoiding", "$\\beta$ Intrusivity", "$\\beta$ Hyperarousal", 
  "$\\sigma$ Avoiding", "$\\sigma$ Intrusivity", "$\\sigma$ Hyperarousal", 
  "$\\alpha$ Avoiding", "$\\alpha$ Intrusivity", "$\\alpha$ Hyperarousal"
)
colnames(summary_mod4) <- c("mean","SE", "lower bound", "upper bound", "Rhat", "ESS_Bulk", "ESS_Tail")

summary_mod4 %<>%
  select(-(c(ESS_Bulk, ESS_Tail))) %>% # removing ESS
  rownames_to_column(var = "parameter")

write.csv(
  summary_mod4, 
  file = here::here("reports", "suppl_mat", "suppl_tbls", "tbl_4.csv"), 
  row.names = FALSE
)


# eof ---

