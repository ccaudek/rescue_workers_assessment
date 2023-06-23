# Script name: 02_run_mplus_scs.R
# Project: self-compassion
# Script purpose: SCS scale dimensionality
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Oct  5 07:24:18 2022
# Last Modified Date: Thu Oct 27 10:38:39 2022
# 
# Notes: The MLR estimator will be used here and in the because in the SEM  
# analyses because the subscales test scores (used in SEM) cannot be considered 
# categorical.
#
# Mplus reliability computed from here: 
# https://hectornajera83.github.io/book/Chapter-3.html


library("here")
library("tidyverse")
library("MplusAutomation")
library("gt")
library("glue")
library("kableExtra")
library("misty")

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6 # Use 6 cores
)

source(
  here::here(
    "src", "R", "functions", "funs_add_neoffi60_subscales.R"
  )
)

# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here(
  "src", "R", "scripts", "02_mplus", "mplus_models", "scs"
)


# Save data for Mplus -----------------------------------------------------

# Read the complete data.
d <- readRDS(
  file = here("data", "processed", "all_items", "rescue_workers_final.Rds")
)
nrow(d)

# Add NEO-FFI-60 subscales.
d1 <- add_neoffi60_subscales(d)

d2 <- d1 |> 
  dplyr::filter(FLAG_1 == "keep") # & FLAG_2 == "keep"

d2 |> 
  group_by(group) |> 
  summarise(
    n = n()
  )

# Add grp.
d2$grp <- ifelse(
  d2$group == "rescue_workers", "rescue_workers", "community_sample"
) |> 
  factor()

# d2$grp <- ifelse(d2$grp == "rescue_workers", 0, 1)

# Get data of the RW group only.
rw_df <- d2 |> 
  dplyr::filter(group == "rescue_workers")

# Use data of all groups!
sem_df <- d2 |> 
  dplyr::select(
    # SCS
    all_of(contains("scs_")), 
    # PTGI
    appreciation_of_life, new_possibilities, personal_strength, spirituality, 
    relating_to_others,
    # IES-R
    avoiding, intrusivity, hyperarousal,
    # COPE-NVI
    positive_attitude, problem_orientation,
    # MSPSS
    family, friends, significant_other,
    # NEO-FFI-60 Neuroticism
    negative_affect, self_reproach,
    # NEO-FFI-60 Extraversion
    positive_affect, sociability, activity
  )

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  sem_df, 
  file = here::here(
    "src", "R", "scripts", "02_mplus", "mplus_models", "scs", "semdata.dat"
  )
)


omega_t <- list()

# Model 1a: One-factor CFA ------------------------------------------------

runModels(paste0(the_dir, "/m1a.inp"), showOutput = TRUE)
m1a <- MplusAutomation::readModels(paste0(the_dir, "/m1a.out"))
summary(m1a)
# 1 factor CFA.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 130 
# 
# Model: Chi2(df = 299) = 10697.252, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.649, TLI = 0.618, SRMR = 0.149 
# RMSEA = 0.18, 90% CI [0.178, 0.183], p < .05 = 0 

lambdas <- m1a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:26, 3]
e <- m1a$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[1]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[1]]) <- "m1a"
omega_t[[1]]


# Model 1b: One-factor ESEM -----------------------------------------------

runModels(paste0(the_dir, "/m1b.inp"), showOutput = TRUE)
m1b <- MplusAutomation::readModels(paste0(the_dir, "/m1b.out"))
summary(m1b)
# Model 1b: One-factor ESEM.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 130 
# 
# Model: Chi2(df = 299) = 10697.251, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.649, TLI = 0.618, SRMR = 0.149 
# RMSEA = 0.18, 90% CI [0.178, 0.183], p < .05 = 0 

lambdas <- m1b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:26, 3]
e <- m1b$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[2]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[2]]) <- "m1b"
omega_t[[2]]


# Model 2a: Two-factor CFA ------------------------------------------------

runModels(paste0(the_dir, "/m2a.inp"), showOutput = TRUE)
m2a <- MplusAutomation::readModels(paste0(the_dir, "/m2a.out"))
summary(m2a)
# Model 2a: Two-factor CFA.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 131 
# 
# Model: Chi2(df = 298) = 4866.771, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.846, TLI = 0.832, SRMR = 0.088 
# RMSEA = 0.12, 90% CI [0.117, 0.123], p < .05 = 0

lambdas <- m2a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:26, 3]
e <- m2a$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[3]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[3]]) <- "m2a"
omega_t[[3]]


# Model 2b: Two-factor ESEM -----------------------------------------------

runModels(paste0(the_dir, "/m2b.inp"), showOutput = TRUE)
m2b <- MplusAutomation::readModels(paste0(the_dir, "/m2b.out"))
summary(m2b)
# Model 2b: Two-factor ESEM.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 155 
# 
# Model: Chi2(df = 274) = 4814.516, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.847, TLI = 0.818, SRMR = 0.059 
# RMSEA = 0.125, 90% CI [0.121, 0.128], p < .05 = 0 

lambdas <- m2b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:52, 3]
e <- m2b$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[4]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[4]]) <- "m2b"
omega_t[[4]]


# Model 3a: Six-factor CFA ------------------------------------------------

runModels(paste0(the_dir, "/m3a.inp"), showOutput = TRUE)
m3a <- MplusAutomation::readModels(paste0(the_dir, "/m3a.out"))
summary(m3a)
# Model 3a: Six-factor CFA.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 145 
# 
# Model: Chi2(df = 284) = 3168.562, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.903, TLI = 0.889, SRMR = 0.067 
# RMSEA = 0.098, 90% CI [0.094, 0.101], p < .05 = 0 

lambdas <- m3a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:26, 3]
e <- m3a$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[5]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[5]]) <- "m3a"
omega_t[[5]]


# Model 3b: Six-factor ESEM -----------------------------------------------

runModels(paste0(the_dir, "/m3b.inp"), showOutput = TRUE)
m3b <- MplusAutomation::readModels(paste0(the_dir, "/m3b.out"))
summary(m3b)
# Model 3b: Six-factor ESEM.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 245 
# 
# Model: Chi2(df = 184) = 691.244, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.983, TLI = 0.97, SRMR = 0.016 
# RMSEA = 0.051, 90% CI [0.047, 0.055], p < .05 = 0.364 

lambdas <- m3b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:156, 3]
e <- m3b$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[6]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[6]]) <- "m3b"
omega_t[[6]]


# Model 4a: Bifactor-CFA (1 G- and 6 S-factors) ---------------------------

runModels(paste0(the_dir, "/m4a.inp"), showOutput = TRUE)
m4a <- MplusAutomation::readModels(paste0(the_dir, "/m4a.out"))
summary(m4a)
# Model 4a: Bifactor-CFA (1 G- and 6 S-factors).Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 156 
# 
# Model: Chi2(df = 273) = 6520.692, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.789, TLI = 0.749, SRMR = 0.111 
# RMSEA = 0.146, 90% CI [0.143, 0.149], p < .05 = 0 

l <- lambdas[1:52, 3]
lambdas <- m4a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
# Reliability

e1 <- m4a$parameters$r2[, 2]
e <- ifelse(is.na(e1), mean(e1, na.rm = TRUE), e1)
lambda_2 <- sum(l)^2
error <- sum(e, na.rm = TRUE)
omega_t[[7]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[7]]) <- "m4a"
omega_t[[7]]


# Model 4b: Bifactor-ESEM (1 G- and 6 S-factors) --------------------------

runModels(paste0(the_dir, "/m4b.inp"), showOutput = TRUE)
m4b <- MplusAutomation::readModels(paste0(the_dir, "/m4b.out"))
summary(m4b)
# Model 4b: Bifactor-ESEM (1 G- and 6 S-factors).Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 265 
# 
# Model: Chi2(df = 164) = 542.343, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.987, TLI = 0.975, SRMR = 0.014 
# RMSEA = 0.046, 90% CI [0.042, 0.051], p < .05 = 0.907 

lambdas <- m4b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
l <- lambdas[1:156, 3]
e <- m4b$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[8]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[8]]) <- "m4b"
omega_t[[8]]

l_sc <- lambdas[1:26, 3] |> abs()
omega_h <- sum(l_sc)^2 / (lambda_2 + error)
omega_h

# Model 5a: Two-bifactor (two-tier) CFA model (2 G- and 6 S-factors) ------

runModels(paste0(the_dir, "/m5a.inp"), showOutput = TRUE)
m5a <- MplusAutomation::readModels(paste0(the_dir, "/m5a.out"))
summary(m5a)
# Model 5a: Two-bifactor (two-tier) CFA model (2 G- and 6 S-factors).Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 157 
# 
# Model: Chi2(df = 272) = 2732.123, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.917, TLI = 0.901, SRMR = 0.07 
# RMSEA = 0.092, 90% CI [0.089, 0.095], p < .05 = 0 

lambdas <- m5a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
# Reliability
l <- lambdas[1:52, 3]
e1 <- m5a$parameters$r2[, 2]
e <- ifelse(is.na(e1), mean(e1, na.rm = TRUE), e1)
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[9]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[9]]) <- "m5a"
omega_t[[9]]


# Model 5b: Two-bifactor (two-tier) ESEM model (2 G- and 6 S-factors) ------

runModels(paste0(the_dir, "/m5b.inp"), showOutput = TRUE)
m5b <- MplusAutomation::readModels(paste0(the_dir, "/m5b.out"))
summary(m5b)
# Model 5b: Two-bifactor (two-tier) ESEM model (2 G- and 6 S-factors).Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 272 
# 
# Model: Chi2(df = 157) = 465.613, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.99, TLI = 0.978, SRMR = 0.013 
# RMSEA = 0.043, 90% CI [0.038, 0.047], p < .05 = 0.995 

lambdas <- m5b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
# Reliability
l <- lambdas[1:182, 3]
e1 <- m5b$parameters$r2[, 2]
e <- ifelse(is.na(e1), mean(e1, na.rm = TRUE), e1)
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[10]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[10]]) <- "m5b"
omega_t[[10]]


# Model 6a: Bifactor ESEM model (1 G- and 2 S-factors) --------------------

runModels(paste0(the_dir, "/m6a.inp"), showOutput = TRUE)
m6a <- MplusAutomation::readModels(paste0(the_dir, "/m6a.out"))
summary(m6a)
# Model 6a: Bifactor ESEM model (1 G- and 2 S-factors).Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 179 
# 
# Model: Chi2(df = 250) = 2736.704, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.916, TLI = 0.891, SRMR = 0.037 
# RMSEA = 0.097, 90% CI [0.093, 0.1], p < .05 = 0 

lambdas <- m6a$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
# Reliability
l <- lambdas[1:78, 3]
e <- m6a$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[11]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[11]]) <- "m6a"
omega_t[[11]]


# Model 6b: Bif. ESEM (1 G- and 2 S-factors) residual corr. ---------------

runModels(paste0(the_dir, "/m6b.inp"), showOutput = TRUE)
m6b <- MplusAutomation::readModels(paste0(the_dir, "/m6b.out"))
summary(m6b)
# Model 6b: Bifactor ESEM model (1 G- and 2 S-factors) correlated residuals.Estimated using WLSMV 
# Number of obs: 1068, number of (free) parameters: 183 
# 
# Model: Chi2(df = 246) = 1444.52, p = 0 
# Baseline model: Chi2(df = 325) = 29932.742, p = 0 
# 
# Fit Indices: 
#   
#   CFI = 0.96, TLI = 0.947, SRMR = 0.029 
# RMSEA = 0.068, 90% CI [0.064, 0.071], p < .05 = 0 

lambdas <- m6b$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
# Reliability
l <- lambdas[1:78, 3]
e <- m6b$parameters$r2[, 2]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[12]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[12]]) <- "m6a"
omega_t[[12]]

# saveRDS(omega_t, here("data", "processed", "scs_omega_total.Rds"))



# eof ---





# compareModels(
#   m3b,
#   m5b,
#   show = "all",
#   equalityMargin = c(param = 1e-04, pvalue = 1e-04),
#   compare = "unstandardized",
#   sort = "none",
#   showFixed = FALSE,
#   showNS = FALSE,
#   diffTest = TRUE
# )

runModels(paste0(the_dir, "/m4c.inp"), showOutput = TRUE)
m4c <- MplusAutomation::readModels(paste0(the_dir, "/m4c.out"))
summary(m4c)
