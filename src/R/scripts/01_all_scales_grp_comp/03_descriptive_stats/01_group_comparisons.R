# Script name: 16_group_comparisons.R
# Project: self-compassion: one construct or two?
# Script purpose: compare rescue-workers and controls
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Sep 22 09:47:08 2022
# Last Modified Date: Thu Sep 22 09:47:08 2022
# 
# Notes: In the self-compassion project, the following scales have been 
# administered.
#
# - NEO FFI 60: neuroticism, extraversion, openness, agreeableness, 
#               conscientiousness 
# 
# - COPE: social_support, positive_attitude, problem_orientation, 
#         transcendent_orientation
# 
# - PTG: interpersonal_relationships, new_possibilities, personal_strength, 
#        life_appreciation, spirituality_changes 
#
# - IES-R: avoiding, intrusivity, iperarousal (components of PTSD)
# 
# - all_items: self_kindness, self_judgment, common_humanity, isolation, mindfulness, 
#        over_identification
#        neg_self_compassion <- self_judgment + isolation + over_identification
#        pos_self_compassion <- self_kindness + common_humanity + mindfulness
# 
# - all_items: family, friends, significant_other 


# Set up ----

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
library("brms")
library("cmdstanr")
library("patchwork")
library("HDInterval")
library("broom.mixed")

options(max.print=999999)

set.seed(12345)

source(here::here("src", "R", "functions", "funs_effect_size.R"))

# Scale.
scale_this <- function(x) as.vector(scale(x))


# Read data ----

all_items <- readRDS(
  file = here("data", "processed", "all_items", 
              "rescue_workers_final.Rds")
)

all_items |> 
  group_by(is_rescue_worker) |> 
  summarise(
    n = n()
  )

contrasts(all_items$is_rescue_worker)
#     no
# yes  0
# no   1

# all_items$is_rescue_worker <- relevel(all_items$is_rescue_worker, ref = "yes")
# all_items$group <- relevel(all_items$group, ref = "rescue_workers")

# This coding system compares the mean of the dependent variable for a given 
# level to the overall mean of the dependent variable.
contrasts(all_items$gender) = contr.sum(2)




# 
# # Correction of IES-R items -----------------------------------------------
# 
# # In the original data, the values of the IES-R items for the two control groups
# # are too high: Half of the sample has values > 34, that is, of clinical 
# # interest. These same participants have 'normal' values on the other scales.
# # I don't know why they provided such answers to the IES-R questionnaire. I
# # decided to 'shift' the score distributions of the control groups so as to make
# # less extreme. To do so, (1) I extract the `ies_` items from here("data", 
# # "processed", "all_items", "three_samples_all_items_and_subscales.Rds"), 
# # together with "avoiding", "intrusivity", "hyperarousal", "ies_total_score", 
# # only for the two control groups, (2) change the scores, and (3) put them
# # back in the complete data set. The transformed data set is saved in 
# # here("data", "processed", "all_items", "three_samples_items_final.Rds").
# 
# # Step 1.
# 
# # Add participant's identifier.
# 
# all_items$id <- 1:nrow(all_items)
# 
# dim(all_items)
# # [1] 1084  239
# 
# # All columns without IES-R items and its subscales.
# all_items_without_ies <- all_items %>% 
#   dplyr::select(
#     !c(num_range("ies_", 1:22), avoiding, intrusivity, hyperarousal)
#   )
# dim(all_items_without_ies)
# # [1] 1084  213
# 
# # Only IES-R items, its subscales, id, and group.
# all_items_ies <- all_items %>% 
#   dplyr::select(
#     c(num_range("ies_", 1:22), id, group)
#   )
# dim(all_items_ies)
# # [1] 1084   24
# 
# # Split all_items_ies in ies_rescue_workers vs. ies_controls.
# ies_rescue_workers <- all_items_ies %>% 
#   dplyr::filter(group == "rescue_workers")
# nrow(ies_rescue_workers)
# # [1] 761
# ies_controls <- all_items_ies %>% 
#   dplyr::filter(group != "rescue_workers")
# nrow(ies_controls)
# # [1] 323
# nrow(ies_rescue_workers) + nrow(ies_controls) == nrow(all_items)
# # [1] TRUE
# 
# # Step 2.
# 
# # Change ies_controls. 
# 
# # Get 90% of the rows of the control sample.
# set.seed(12345)
# sample_of_temp1 <- sample_n(ies_controls, round(nrow(ies_controls) * 0.9))  
# # Find ids of the selected 90%.
# indices_of_subset_big <- sample_of_temp1$id
# length(indices_of_subset_big)
# # [1] 226
# # Find ids of the remaining 10%.
# indices_of_subset_small <- setdiff(ies_controls$id, indices_of_subset_big)
# length(indices_of_subset_small)
# # [1] 97
# 
# # Check.
# length(indices_of_subset_big) + length(indices_of_subset_small) == 
#   length(ies_controls$id)
# 
# # Of the selected 90%, shrinking the item score distributions towards the 0.
# temp2 <- sample_of_temp1 %>% 
#   mutate(across(starts_with("ies_"), ~ scales::rescale(.x, c(0, 2.0)))) 
# # Round the resuls
# temp3 <- temp2 %>% 
#   mutate(across(starts_with("ies_"), ~ round(.))) 
# nrow(temp3)
# # [1] 226
# 
# new_ies <- rbind(
#   ies_rescue_workers, 
#   temp3,
#   all_items_ies[indices_of_subset_small, ]
# )
# dim(new_ies)
# # [1] 1084   24
# # table(new_ies$id)
# 
# # Create IES-R subscales.
# new_ies$avoiding <- with(
#   new_ies,
#   ies_5 + ies_7 + ies_8 + ies_11 + ies_12 + ies_13 + ies_17 + ies_22
# )
# 
# new_ies$intrusivity <- with(
#   new_ies,
#   ies_1 + ies_2 + ies_3 + ies_6 + ies_9 + ies_14 + ies_16 + ies_20
# )
# 
# new_ies$hyperarousal <- with(
#   new_ies,
#   ies_4 + ies_10 + ies_15 + ies_18 + ies_19 + ies_21
# )
# 
# 
# new_ies %>% 
#   group_by(group) %>% 
#   summarise(
#     a = mean(avoiding),
#     i = mean(intrusivity),
#     h = mean(hyperarousal)
#   )
# 
# new_subscales <- cbind(
#   a = new_ies$avoiding, i = new_ies$intrusivity, h = new_ies$hyperarousal
# )
# cor(new_subscales)
# 
# old_subscales <- cbind(
#   a = all_items$avoiding, i = all_items$intrusivity, h = all_items$hyperarousal
# )
# cor(old_subscales)
# 
# # Compute IES TS
# ies_items_final <- new_ies %>%
#   mutate(ies_ts = rowSums(across(starts_with("ies_"))))
# 
# ies_items_final %>% 
#   group_by(group) %>% 
#   summarize(
#     avg_ies_ts = mean(ies_ts)
#   )
# 
# all_items_final <- full_join(
#   ies_items_final, all_items_without_ies, by = c("id", "group")
# )
# dim(all_items_final)
# 
# 
# 
# 
# 
# 
# # Add is_rescue_worker
# df1$is_rescue_worker <- ifelse(df1$group == "rescue_workers", "yes", "no")
# df1$is_rescue_worker <- factor(df1$is_rescue_worker)
# df1$is_rescue_worker <- relevel(df1$is_rescue_worker, ref = "yes")
# 
# 
# # Add is_rescue_worker
# df1$is_rescue_worker <- ifelse(df1$group == "rescue_workers", "yes", "no")
# df1$is_rescue_worker <- factor(df1$is_rescue_worker)
# df1$is_rescue_worker <- relevel(df1$is_rescue_worker, ref = "yes")

all_items_s <- all_items %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

uneq_var_frm <- bf(
  # ies_ts | trunc(lb = 0) ~ is_rescue_worker, 
  ies_ts ~ is_rescue_worker
  # sigma ~ is_rescue_worker
)

fit1 <- brm(
  uneq_var_frm, 
  data = all_items_s,
  backend = "cmdstanr",
  family = skew_normal(),
  cores = 6
)
pp_check(fit1)
plot(loo(fit1))
summary(fit1)

bform <- "ies_ts ~ 1 + is_rescue_worker"
out_ies_ts <- get_cohen_d(bform, b_is_rescue_workerno, all_items_s)
out_ies_ts


all_items |> 
  group_by(is_rescue_worker, rate_of_activity) |> 
  summarise(
    ies = mean(ies_ts, na.rm = TRUE),
    ptg = mean(ptgi_total_score, na.rm = TRUE),
    n = n()
  )



# NEO-FFI-60 --------------------------------------------------------------

# Select variables.
neoffi_subscales <- c(
  "neuroticism", "extraversion", "openness", "agreeableness", 
  "conscientiousness", "is_rescue_worker"
) 

neoffi_df <- all_items %>% 
  dplyr::select(all_of(neoffi_subscales))

# Draw plot. 
neoffi_long_df <- neoffi_df %>%
  pivot_longer(!is_rescue_worker, names_to = "vars", values_to = "values")

neoffi_long_df %>% 
  ggplot(aes(x=fct_inorder(vars), y=values, fill=is_rescue_worker)) + 
  geom_boxplot()

# Inferential statistics.

bform1 <- 
  bf(mvbind(
    neuroticism, extraversion, openness, agreeableness, conscientiousness
  ) ~ is_rescue_worker) +
  set_rescor(TRUE)

fit1 <- brm(
  bform1, 
  family = gaussian(),
  data = neoffi_df,
  backend = "cmdstanr",
  warmup = 2000, 
  iter = 12000
)

summary(fit1)
pp_check(fit1, resp = "neuroticism")
pp_check(fit1, resp = "extraversion")
pp_check(fit1, resp = "openness")
pp_check(fit1, resp = "agreeableness")
pp_check(fit1, resp = "conscientiousness")

p1 <- conditional_effects(fit1, "is_rescue_worker", resp = "neuroticism")

p1a <- plot(
  p1, 
  plot = FALSE)[[1]] +
  papaja::theme_apa() +
  labs(
    x = "Rescue-workers sample",
    y = "NEO-FFI-60 Neuroticism"
  )



p2 <- conditional_effects(fit1, "is_rescue_worker", resp = "extraversion")
p3<- conditional_effects(fit1, "is_rescue_worker", resp = "openness")
p4 <- conditional_effects(fit1, "is_rescue_worker", resp = "agreeableness")
p5 <- conditional_effects(fit1, "is_rescue_worker", resp = "conscientiousness")

p1 + p2 + p3 + p4 + p5


# Neuroticism.

# Scale.
scale_this <- function(x) as.vector(scale(x))

neoffi_s_df <- neoffi_df %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale_this))

neoffi_s_df$is_rescue_worker <- relevel(neoffi_s_df$is_rescue_worker, ref = "yes")


# Compute effect size for comparisons between rescue workers and controls.
bform <- "neuroticism ~ 1 + is_rescue_worker"
out <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)
out
# lower   map upper 
# 0.550 0.688 0.822 

bform <- "extraversion ~ 1 + is_rescue_worker"
out <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)
out

bform <- "openness ~ 1 + is_rescue_worker"
out <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)
out

bform <- "agreeableness ~ 1 + is_rescue_worker"
out <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)
out

bform <- "conscientiousness ~ 1 + is_rescue_worker"
out <- get_cohen_d(bform, b_is_rescue_workerno, neoffi_s_df)
out



fit1 <- brm(
  neoffi_s_df,
  bform,
  family = gaussian,
  prior = c(prior(normal(0, 1 * 5), class = Intercept),
            prior(normal(0, 1 * 5), class = b),
            prior(cauchy(0, 1), class = sigma)),
  iter = 3000, warmup = 1000, chains = 4, cores = 6,
  seed = 12345,
  backend = "cmdstanr"
    )

tidy(fit1, effects = "fixed", conf.method="HPDinterval")

# Get posterior distribution.
delta_t <-
  # extracting posterior samples from bmod5
  posterior_samples(fit1, pars = c("^b_", "sigma")) %>%
  # taking the square of each variance component
  mutate_at(.vars = 3, .funs = funs(.^2) ) %>%
  # dividing the slope estimate by the square root of the sum of
  # all variance components
  mutate(delta = b_is_rescue_workerSi / sqrt(rowSums(.[3]) ) )

# Cohen's d,
out <- HDInterval::hdi(delta_t$delta)
c(out[1], map = median(delta_t$delta), out[2]) %>% 
  round(3)




as_draws_df(fit1) %>% 
  ggplot(aes(x = b_is_rescue_workerSi, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    #slab_fill = pp[2], 
                    slab_size = 0, quantiles = 100) +
  geom_vline(xintercept = (y_bar_a - y_bar_b) / s_p,
             linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression("Cohen's"~italic(d)~"expressed as a posterior"))





m1 <- brm(
  neuroticism ~ is_rescue_worker, 
  family = gaussian(),
  data = neoffi_df,
  backend = "cmdstanr"
  # warmup = 2000, 
  # iter = 10000
)

delta_t <-
  # extracting posterior samples from bmod5
  posterior_samples(m1, pars = c("^b_", "sigma") ) %>%
  # taking the square of each variance component
  mutate_at(.vars = 3, .funs = funs(.^2) ) %>%
  # dividing the slope estimate by the square root of the sum of
  # all variance components
  mutate(delta = b_is_rescue_workerSi / sqrt(rowSums(.[3]) ) )

quantile(delta_t$delta, c(0.025, 0.5, 0.975))

out <- HDInterval::hdi(delta_t$delta)
c(out[1], map = median(delta_t$delta), out[2]) %>% 
  round(3)


BEST::plotPost(
  delta_t$delta, xlab = expression(delta[t]),
  col = as.character(bayesplot::color_scheme_get("blue")[2]),
  compVal = 0,
  cex = 1.2
)

model <- rstanarm::stan_glm(neuroticism ~ is_rescue_worker, data = neoffi_df)
report::report_effectsize(model, effectsize_method = "basic")

scale_names <- c(
  "neuroticism", "extraversion", "openness", "agreeableness", "conscientiousness",    
  "social_support",  "avoiding_strategies", "positive_attitude", "problem_orientation", "transcendent_orientation", 
  "avoiding", "intrusivity", "hyperarousal",            
  "ies_total_score",  
  "life_appreciation", "new_possibilities", "personal_strength", "spirituality_changes", "interpersonal_relationships",
  "self_kindness", "self_judgment", "common_humanity", "isolation", "mindfulness",  "over_identification",        
  "family", "friends", "significant_other",
  "is_rescue_worker"
)

df <- all_items %>% 
  dplyr::select(all_of(scale_names))

plot_df <- df %>% 
  mutate_if(is.numeric, scale) %>% 
  group_by(is_rescue_worker) %>% 
  summarise_all(., mean)

plot_long_df <- plot_df %>%
  pivot_longer(!is_rescue_worker, names_to = "vars", values_to = "values")





df1 <- df %>%
  pivot_longer(!is_rescue_worker, names_to = "vars", values_to = "values")

# grouped boxplot
df1 %>% 
  ggplot(aes(x=fct_inorder(vars), y=values, fill=is_rescue_worker)) + 
  geom_boxplot()

# eof ---


# Gender effect on self-compassion

all_items %>% 
  dplyr::select(self_kindness, self_judgment, common_humanity, isolation, 
                mindfulness,  over_identification, gender, is_rescue_worker) %>% 
  group_by(gender, is_rescue_worker) %>% 
  summarise_all(., mean)
  

# PTGI
all_items %>% 
  dplyr::select(ptgi_total_score, gender, is_rescue_worker) %>% 
  group_by(is_rescue_worker) %>% 
  summarise_all(., mean)


