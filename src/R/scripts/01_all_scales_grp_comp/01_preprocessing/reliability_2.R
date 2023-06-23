
lpa_scales <- c(
  "is_rescue_worker",
  "neuroticism", "extraversion", "openness", "agreeableness", "conscientiousness",
  "active_coping", "avoidance_coping", "soc_emo_coping",
  "iesr_ts",
  # "avoiding", "intrusivity", "hyperarousal",
  # "sk", "ch", "mi", "sj", "is", "oi",
  # "pos_sc",
  # "neg_sc",
  # "ts_sc",
  "mpss_tot"
  # "ptgi_total_score"
  # "relating_to_others",
  # "new_possibilities",
  # "personal_strength",
  # "appreciation_of_life",
  # "spirituality"
)


mydf <- data.frame(
  scs = scale(rw_df$scs_ts),
  class = scale(ifelse(rw_df$class == 2, 0.5, -0.5)),
  ptgi = scale(rw_df$ptgi_total_score),
  psc = scale(rw_df$sk + rw_df$ch + rw_df$mi),
  nsc = scale(rw_df$sj + rw_df$oi + rw_df$is),
  commettee = rw_df$red_cross_commeetee_location
)

mydf <- mydf[complete.cases(mydf), ]


f1 <- bf(scs ~ class + (1 + class | commettee), family = gaussian())
f2 <- bf(ptgi ~ scs + class + (1 + class | commettee), family = skew_normal())
mod <- brm(
  f1 + f2 + set_rescor(FALSE), 
  data = mydf, 
  cores = 4, 
  refresh = 0,
  backend = "cmdstanr",
  adapt_delta = 0.99
)

bayestestR::mediation(mod, mediator = "scs", ci = 0.95, method = "SPI")


cor(rw_df$ies_total_score, rw_df$ptgi_total_score)

library("bmlm")

fit <- mlm(d = mydf, 
           id = "commettee",
           x = "class",
           m = "scs",
           y = "ptgi",
           iter = 2000, 
           cores = 4)


mlm_path_plot(fit, level = .95, text = T,
              xlab = "Resilience\nProfile",
              mlab = "Self\nCompassion",
              ylab = "PTG", digits = 2)



model <- '
    active_cop =~ NA*pos_reinterpretation + active_coping + 
                  suppr_competing_activities + planning +
                  restraint + seeking_instrumental_support + acceptance
    soc_emo_cop =~ NA*seeking_instrumental_support + 
                    seeking_emotional_support + venting
    avoidance_cop =~ NA*mental_disengagement + denial + humor + 
                     behavioral_disengagement + substance_use 
                     # religion 
    
    active_cop ~~ 1*active_cop
    soc_emo_cop ~~ 1*soc_emo_cop
    avoidance_cop ~~ 1*avoidance_cop
    
    behavioral_disengagement	~~	venting	
    behavioral_disengagement	~~	substance_use

'

fit1 <- cfa(model, data = all_items)
summary(fit1, standardized = TRUE)

semTools::compRelSEM(fit1)


ac_df <- tibble(
  all_items$pos_reinterpretation, all_items$active_coping, 
  all_items$suppr_competing_activities, all_items$planning,
  all_items$restraint, all_items$seeking_instrumental_support, all_items$acceptance
)
psych::omega(ac_df)
# Alpha:                 0.82 
# G.6:                   0.94 
# Omega Hierarchical:    0.81 
# Omega H asymptotic:    0.91 
# Omega Total            0.89 

ec_df <- tibble(
  all_items$seeking_instrumental_support,
  all_items$seeking_emotional_support, all_items$venting
)
psych::omega(ec_df)
# Alpha:                 0.69 
# G.6:                   0.65 
# Omega Hierarchical:    0.53 
# Omega H asymptotic:    0.69 
# Omega Total            0.77 


vc_df <- tibble(
  all_items$mental_disengagement, all_items$denial, all_items$humor, 
  all_items$behavioral_disengagement, all_items$substance_use
)
psych::omega(vc_df)
# Alpha:                 0.78 
# G.6:                   0.75 
# Omega Hierarchical:    0.73 
# Omega H asymptotic:    0.89 
# Omega Total            0.82 
