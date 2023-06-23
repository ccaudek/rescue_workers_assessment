library("here")             # here function
library("tidyverse")        # ggplot, dplyr, and friends
library("lavaan")           # SEM functions
library("misty")            # item.reverse() function

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6  # Use 6 cores
)

source(here::here("src", "R", "functions", "funs_second_pass_data_clean.R"))
source(here::here("src", "R", "functions", "funs_add_neoffi60_subscales.R"))


# Get data ----------------------------------------------------------------

# Perform the second pass and generate the final cleaned data-set.
# second_pass_data_clean()

# Read data.
d <- readRDS(
  file = here("data", "processed", "all_items", "rescue_workers_final.Rds")
)
nrow(d)

# Add NEO-FFI-60 subscales.
d1 <- add_neoffi60_subscales(d1)

# Clean data
d2 <- d1 |> 
  dplyr::filter(FLAG_1 == "keep")

d2$grp <- ifelse(
  all_items$group == "rescue_workers", "rescue_workers", "community_sample"
) |>
  factor()


# Only rescue workers -----------------------------------------------------

# Get data of the RW group only.
rw_dat <- d2 |> 
  dplyr::filter(group == "rescue_workers")


# SEM analysis ------------------------------------------------------------

# Model for a single group.

model1 <- "
  # post-traumatic growth
  ptg =~ appreciation_of_life + new_possibilities + 
         personal_strength + spirituality + 
         relating_to_others
         
  # ptsd
  pts =~ avoiding + intrusivity + hyperarousal
  
  # coping
  cop =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # NEO-FFI-60
  neu =~ negative_affect + self_reproach
  ext =~ positive_affect + sociability + activity
  # ope =~ aesthetic_interests + intellectual_interests + unconventionality
  # agr =~ nonantagonistic_orientation + prosocial_orientation 
  # con =~ orderliness + goal_striving + dependability

  # regressions
  ptg ~ dg_cope*cop + dg_soc*soc + dg_neuro*neu + dg_ext*ext 
  pts ~ ds_cope*cop + ds_soc*soc + ds_neuro*neu + ds_ext*ext  

  nsc ~ nsc_cope*cop + nsc_soc*soc + nsc_neuro*neu + nsc_ext*ext 
  psc ~ psc_cope*cop + psc_soc*soc + psc_neuro*neu + psc_ext*ext 
  
  ptg ~ ig_nsc*nsc + ig_psc*psc 
  pts ~ is_nsc*nsc + is_psc*psc
  
  # covariances
  self_judgment ~~ self_kindness
"

fit1 <- sem(
  model1,
  data = rw_dat,
  estimator = "MLM",
  std.lv = TRUE,
  group = "grp"
)

fit_meas_m1 <- fitMeasures(
  fit1, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_m1
# chisq         df    cfi   cfi.robust     nfi        tli tli.robust      rmsea       srmr 
# 766.420    224.000  0.939      0.942   0.917      0.925      0.928      0.059      0.053 

summary(fit1, standardized = TRUE, rsquare = TRUE)


# Both groups ---------------------------------------------------------

# Different loadings and regression coefficients for the two groups.
model2 <- "
  # post-traumatic growth
  ptg =~ appreciation_of_life + new_possibilities + 
         personal_strength + spirituality + 
         relating_to_others
         
  # ptsd
  pts =~ avoiding + intrusivity + hyperarousal
  
  # coping
  cop =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # neuroticism
  neu =~ negative_affect + self_reproach
  ext =~ positive_affect + sociability + activity

  # regressions

  # direct effects on ptg, pts
  ptg ~ c(dg_cope_1, dg_cope_2)*cop + c(dg_soc_1, dg_soc_2)*soc + 
        c(dg_neuro_1, dg_neuro_2)*neu + c(dg_ext_1, dg_ext_2)*ext
  pts ~ c(ds_cope_1, ds_cope_2)*cop + c(ds_soc_1, ds_soc_2)*soc + 
        c(ds_neuro_1, ds_neuro_2)*neu + c(ds_ext_1, ds_ext_2)*ext

  # direct effects of IVs on negative sc and positiv sc
  nsc ~ c(nsc_cope_1, nsc_cope_2)*cop + c(nsc_soc_1, nsc_soc_2)*soc + 
        c(nsc_neuro_1, nsc_neuro_2)*neu + c(nsc_ext_1, nsc_ext_2)*ext
  psc ~ c(psc_cope_1, psc_cope_2)*cop + c(psc_soc_1, psc_soc_2)*soc + 
        c(psc_neuro_1, psc_neuro_2)*neu + c(psc_ext_1, psc_ext_2)*ext ###
  
  # mediation effects of negative sc and positiv sc on ptg, pts
  ptg ~ c(ig_nsc_1, ig_nsc_2)*nsc + c(ig_psc_1, ig_psc_2)*psc 
  pts ~ c(is_nsc_1, is_nsc_2)*nsc + c(is_psc_1, is_psc_2)*psc 
  
  # covariances
  self_judgment ~~ self_kindness
"

# configural invariance
fit2 <- sem(
  model2,
  data = d2,
  estimator = "MLM",
  std.lv = TRUE,
  group = "grp"
)

fit_meas_m2 <- fitMeasures(
  fit2, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_m2
# chisq         df     cfi    cfi.robust     nfi     tli tli.robust      rmsea       srmr 
# 1199.975    448.000   0.937      0.940   0.905   0.923      0.926      0.058      0.053 

summary(fit2, standardized = TRUE, rsquare = TRUE)
# modindices(fit2, sort = TRUE, maximum.number = 5)


# weak invariance.
fit3 <- sem(
  model2,
  data = all_items,
  estimator = "MLM",
  std.lv = TRUE,
  group = "grp", 
  group.equal = c("loadings")
)

# strong invariance
fit4 <- sem(
  model2,
  data = all_items,
  estimator = "MLM",
  std.lv = TRUE,
  group = "grp", 
  group.equal = c("intercepts", "loadings")
)

lavTestLRT(fit2, fit3, fit4, method = "satorra.bentler.2010")
#       Df    AIC    BIC  Chisq      Chisq diff Df diff Pr(>Chisq)    
# fit2 448 128337 129318 1200.0                                  
# fit3 464 128354 129256 1248.7      31.39      16      0.01198 *  
# fit4 480 128704 129528 1630.9     394.96      16      < 2e-16 ***

summary(fit4, standardized = TRUE, rsquare = TRUE)



# One single factor for Self-Compassion

model3 <- "
  # post-traumatic growth
  ptg =~ appreciation_of_life + new_possibilities + 
         personal_strength + spirituality + 
         relating_to_others
         
  # ptsd
  pts =~ avoiding + intrusivity + hyperarousal
  
  # coping
  cop =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  sc =~ self_judgment + isolation + over_identification + 
        self_kindness + common_humanity + mindfulness
  
  # neuroticism
  neu =~ negative_affect + self_reproach
  ext =~ positive_affect + sociability + activity

  # regressions

  # direct effects on ptg, pts
  ptg ~ c(dg_cope_1, dg_cope_2)*cop + c(dg_soc_1, dg_soc_2)*soc + 
        c(dg_neuro_1, dg_neuro_2)*neu + c(dg_ext_1, dg_ext_2)*ext
  pts ~ c(ds_cope_1, ds_cope_2)*cop + c(ds_soc_1, ds_soc_2)*soc + 
        c(ds_neuro_1, ds_neuro_2)*neu + c(ds_ext_1, ds_ext_2)*ext

  # direct effects of IVs on negative sc and positiv sc
  sc ~ c(sc_cope_1, sc_cope_2)*cop + c(sc_soc_1, sc_soc_2)*soc + 
       c(sc_neuro_1, sc_neuro_2)*neu + c(sc_ext_1, sc_ext_2)*ext
  
  # mediation effects of sc on ptg, pts
  ptg ~ c(ig_sc_1, ig_sc_2)*sc 
  pts ~ c(is_sc_1, is_sc_2)*sc 
  
  # covariances
  self_judgment ~~ self_kindness
"

fit5 <- sem(
  model3,
  data = d2,
  estimator = "MLM",
  std.lv = TRUE,
  group = "grp"
)

fit_meas_m5 <- fitMeasures(
  fit5, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_m5

summary(fit5, standardized = TRUE, rsquare = TRUE)

lavTestLRT(fit2, fit5, method = "satorra.bentler.2010")
#       Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
# fit2 448 128337 129318 1200.0                                  
# fit5 460 129292 130214 2178.5     192.41      12  < 2.2e-16 ***



controls_df <- all_items |> 
  dplyr::filter(group == "rescue_workers")


mod_bifactor <- '
  gf =~ scs_1 + scs_2 + scs_3 + scs_4 + scs_5 + scs_6 + scs_7 + scs_8 + scs_9 +
        scs_10 + scs_11 + scs_12 + scs_13 + scs_14 + scs_15 + scs_16 + scs_17 + 
        scs_18 + scs_19 + scs_20 + scs_21 + scs_22 + scs_23 + scs_24 + scs_25 + 
        scs_26
  sk =~ scs_5 + scs_12 + scs_19 + scs_23 + scs_26 
  sj =~ scs_1 + scs_8 + scs_11 + scs_16 + scs_21
  ch =~ scs_3 + scs_7 + scs_10 + scs_15
  is =~ scs_4 + scs_13 + scs_18 + scs_25
  mi =~ scs_9 + scs_14 + scs_17 + scs_22
  oi =~ scs_2 + scs_6 + scs_20 + scs_24

  # gf ~~ 0*sk
  # gf ~~ 0*sj
  # gf ~~ 0*ch
  # gf ~~ 0*is
  # gf ~~ 0*mi
  # gf ~~ 0*oi 
  # 
  # sk ~~ 0*sj
  # sk ~~ 0*ch
  # sk ~~ 0*is
  # sk ~~ 0*mi
  # sk ~~ 0*oi 
  # 
  # sj ~~ 0*ch
  # sj ~~ 0*is
  # sj ~~ 0*mi
  # sj ~~ 0*oi 
  # 
  # ch ~~ 0*is
  # ch ~~ 0*mi
  # ch ~~ 0*oi 
  # 
  # is ~~ 0*mi
  # is ~~ 0*oi 
  # 
  # mi ~~ 0*oi

'

fit_bifactor <- lavaan::cfa(
  mod_bifactor,
  data = controls_df,
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
  orthogonal = TRUE
)

summary(
  fit_bifactor, 
  standardized=TRUE, 
  fit.measures=TRUE
)




controls_items <- controls_df |> 
  dplyr::select(all_of(contains("scs_")))

output <- faoutlier::robustMD(controls_items)
bad_ids_1 <- c(
    287 ,
    75  ,
    261 ,
    152 ,
    29  ,
    249 ,
    56  ,
    179 ,
    136 ,
    197 
)



(ORresult <- faoutlier::obs.resid(temp, bifactor_model))
plot(ORresult)

foo <- tibble(
  id = ORresult$id,
  std_res = matrixStats::rowMaxs(ORresult$std_res)
)

bad_ids_2 <- which(foo$std_res > 2.0) |> as.vector()

temp <- controls_items[-c(bad_ids_1, bad_ids_2), ]



scs_fit_controls <- cfa(
  bifactor_model, 
  data = controls_df, 
  std.lv = TRUE,
  ordered = TRUE
) 



fit_meas_scs_controls <- fitMeasures(
  scs_fit_controls, 
  c("chisq", "df", "cfi", "cfi.robust", "nfi", "tli", "tli.robust", 
    "rmsea", "srmr")
)
fit_meas_scs_controls


summary(scs_fit_controls, standardized = TRUE, rsquare = TRUE)

modindices(scs_fit_controls, sort = TRUE, maximum.number = 5)


