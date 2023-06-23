\clearpage

# COPE-NVI

```{r boxplot-copenvi, echo=FALSE, out.width = "100%", fig.cap= "Half-boxplots and score distributions by group (rescue-worker sample vs. community/student sample) for the five COPE-NVI subscales."}
knitr::include_graphics(here::here("reports", "suppl_mat", "suppl_figs", "copenvi_all_subscales.pdf"))
```

```{r copemvmodel, cache = TRUE, echo=FALSE, message = FALSE, warning = FALSE, results = "hide"}
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
```

```{r coefs-copemvmodel, echo=FALSE, message = FALSE, warning = FALSE, results = "asis"}
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

papaja::apa_table(
  summary_mod2,
  placement = "h",
  align = c("l", "r", "r", "r", "r", "r"),
  caption = "Posterior mean, standard error, 95\\% credible interval and $\\hat{R}$
    statistic for each parameter of the model bmod2.",
  note = NULL,
  small = TRUE,
  digits = 3,
  escape = FALSE
)
```

```{r, echo = FALSE}
# social_support, avoiding_strategies, positive_attitude, problem_orientation, transcendent_orientation

# pp_check(fit2, resp = "socialsupport")
# pp_check(fit2, resp = "avoidingstrategies")
# pp_check(fit2, resp = "positive_attitude")
# pp_check(fit2, resp = "problem_orientation")
# pp_check(fit2, resp = "transcendent_orientation")

p1 <- conditional_effects(fit2, "is_rescue_worker", resp = "socialsupport")
p1a <- plot(
  p1, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Social Support"
  )

p2 <- conditional_effects(fit2, "is_rescue_worker", resp = "avoidingstrategies")
p2a <- plot(
  p2, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Avoiding Strategies"
  )

p3 <- conditional_effects(fit2, "is_rescue_worker", resp = "positiveattitude")
p3a <- plot(
  p3, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Positive Attitude"
  )

p4 <- conditional_effects(fit2, "is_rescue_worker", resp = "problemorientation")
p4a <- plot(
  p4, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Problem Orientation"
  )

p5 <- conditional_effects(fit2, "is_rescue_worker", resp = "transcendentorientation")
p5a <- plot(
  p5, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Transcendent Orientation"
  )

neo_ffi_five_panels <- (p1a | p2a | p3a) /
  (p4a | p5a)

ggsave(here::here("reports", "suppl_mat", "suppl_figs", "cope_five_panels.pdf"))
```

(ref:plots-copemvmodel-caption) Posterior parameter estimates from a Bayesian linear model with each subscale of the COPE-NVI as response variable and group (rescue-worker sample vs. community/student sample) as predictor. Vertical bars represent 95% credibility intervals.

```{r plots-copemvmodel, echo=FALSE, out.width = "100%", fig.cap= "(ref:plots-copemvmodel-caption)"}
# knitr::include_graphics(here::here("scripts", "suppl_mat", "suppl_figs", "cope_five_panels.pdf"))
```


\clearpage

# PTGI

```{r, cache = TRUE}
# Create PTGI total score as the sum of all items
temp <- all_items %>%
  dplyr::select(num_range("ptgi_", 1:21)) %>% 
  mutate(
    ptgi_total_score = rowSums(.)
  )
all_items$ptgi_total_score <- temp$ptgi_total_score
rm(temp)

uneq_var_frm <- bf(
  ptgi_total_score | trunc(lb = 0) ~ is_rescue_worker, 
  sigma ~ is_rescue_worker
)

fit3 <- brm(
  uneq_var_frm, 
  data = all_items,
  backend = "cmdstanr",
  family = gaussian(),
  cores = 6
)
# pp_check(fit3)
# loo(fit3)

d <- tibble(
  ptgi_total_score = all_items$ptgi_total_score,
  is_rescue_worker = all_items$is_rescue_worker
)

bform <- "ptgi_total_score ~ 1 + is_rescue_worker"
out_ptgi_tot <- get_cohen_d(bform, b_is_rescue_workerno, d)
out_ptgi_tot
```

```{r}
pp_check(fit3)
summary(fit3)
```



```{r boxplot-ptgi, echo=FALSE, cache = TRUE, out.width = "100%", fig.cap= "Half-boxplots and score distributions by group (rescue-worker sample vs. community/student sample) for the three PTGI subscales."}
knitr::include_graphics(here::here("reports", "suppl_mat", "suppl_figs", "ptgi_all_subscales.pdf"))
```

```{r}
uneq_var_frm <- bf(
  ptgi_total_score ~ is_rescue_worker, 
  sigma ~ is_rescue_worker)

fit3 <- brm(
  uneq_var_frm, 
  data = all_items,
  backend = "cmdstanr",
  family = asym_laplace(),
  # warmup = 2000, 
  # iter = 12000,
  cores = 6
)
```


```{r ptgimvmodel, cache = TRUE, echo=FALSE, message = FALSE, warning = FALSE, results = "hide"}
# Multivariate model.
bf_al <- bf(appreciation_of_life ~ is_rescue_worker)
bf_sp <- bf(spirituality ~ is_rescue_worker, family = asym_laplace())
bf_ps <- bf(personal_strength ~ is_rescue_worker)
bf_np <- bf(new_possibilities ~ is_rescue_worker)
bf_ro <- bf(relating_to_others ~ is_rescue_worker, family = asym_laplace())

fit3 <- brm(
  bf_al + bf_as + bf_sp + bf_np + bf_ro + set_rescor(FALSE), 
  data = all_items,
  backend = "cmdstanr",
  # warmup = 2000, 
  # iter = 12000,
  cores = 6
)
```

