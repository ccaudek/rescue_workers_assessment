```{r echo = FALSE}
# TODO: print table.

# pp_check(fit1, resp = "neuroticism")
# pp_check(fit1, resp = "extraversion")
# pp_check(fit1, resp = "openness")
# pp_check(fit1, resp = "agreeableness")
# pp_check(fit1, resp = "conscientiousness")

p1 <- conditional_effects(fit1, "is_rescue_worker", resp = "neuroticism")
p1a <- plot(
  p1, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Neuroticism"
  )

p2 <- conditional_effects(fit1, "is_rescue_worker", resp = "extraversion")
p2a <- plot(
  p2, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Extraversion"
  )

p3 <- conditional_effects(fit1, "is_rescue_worker", resp = "openness")
p3a <- plot(
  p3, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Openness"
  )

p4 <- conditional_effects(fit1, "is_rescue_worker", resp = "agreeableness")
p4a <- plot(
  p4, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Agreeableness"
  )

p5 <- conditional_effects(fit1, "is_rescue_worker", resp = "conscientiousness")
p5a <- plot(
  p5, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers",
    y = "Conscientiousness"
  )

neo_ffi_five_panels <- (p1a | p2a | p3a) /
  (p4a | p5a)

ggsave(here::here("reports", "suppl_mat", "suppl_figs", "neo_ffi_five_panels.pdf"))
```

(ref:plots-neoffimvmodel-caption) Posterior parameter estimates from a Bayesian linear model with each subscale of the NEO-FFI-60 as response variable and group (rescue-worker sample vs. community/student sample) as predictor. Vertical bars represent 95% credibility intervals.

```{r plots-neoffimvmodel, echo=FALSE, out.width = "100%", fig.cap= "(ref:plots-neoffimvmodel-caption)"}
# knitr::include_graphics(here::here("scripts", "suppl_mat", "suppl_figs", "neo_ffi_five_panels.pdf"))
```