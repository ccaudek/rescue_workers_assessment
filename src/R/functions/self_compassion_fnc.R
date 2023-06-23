
#' Careless responding. Returns the row indices of the input data.frame for 
#' which one of the following functions has flagged the participant as 
#' 'careless': Mahalanobis Distance, Psychometric Synonyms and Antonyms, 
#' Intra-individual Response Variability, The longest consecutive string of 
#' identical responses given by a person.
outliers_carless_responding <- function(df) {
  
  # Mahalanobis Distance
  preds_mahad <- mahad(df, plot = FALSE, flag = TRUE, confidence = 0.99)
  out <- preds_mahad[preds_mahad$flagged == TRUE, ]
  if (dim(out)[1] > 0) {
    out_ind_mahad <- rownames(out) %>% as.numeric()
  } else {
    out_ind_mahad = 0
  }
  
  # Person-total correlation
  cm <- colMeans(df)
  ptc <- apply(df, 1, function(x) cor(x, cm)) 
  # If sd = 0, then we get NA. Replace NAs with 0 (flagged).
  ptc <- ifelse(is.na(ptc), 0, ptc)
  # Negative person-total correlations are flagged.
  out_pers_tot_cor <- which(ptc <= 0)
  
  # Psychometric Synonyms and Antonyms
  # The Psychometric Synonyms index takes variable pairs (these should be items) 
  # that are highly correlated (e.g., r>.60), and in that sense psychometrically 
  # synonymous, and calculates a within-person correlation over these item pairs. 
  # The rationale behind this index is that pairs of items that elicit similar 
  # responses across the population should also do so for each individual. The 
  # function psychsyn_critval() returns a list of all possible correlations 
  # between variables, ordered by magnitude. 
  preds_psychsyn <- try(psychsyn(df), silent=TRUE)
  preds_psychsyn <- ifelse(is.na(preds_psychsyn), 0, preds_psychsyn)
  
  if (is.numeric(preds_psychsyn)) {
    out <- boxplot.stats(preds_psychsyn)$out
    out_ind_psychsyn <- which(preds_psychsyn %in% c(out))
  } 
  
  preds_psychant <- try(psychant(df), silent=TRUE)
  preds_psychant <- ifelse(is.na(preds_psychant), 0, preds_psychant)

  if (is.numeric(preds_psychant)) {
    out <- boxplot.stats(preds_psychant)$out
    out_ind_psychant <- which(preds_psychant %in% c(out))
  } 
  if (!is.numeric(preds_psychant))
    out_ind_psychant = 0
  
  # Intra-individual Response Variability
  # The standard deviation of responses across a set of consecutive item  
  # responses for an individual.
  irv_total <- irv(df)
  # boxplot(irv_total) 
  # Get number of row of data.frame of outliers.
  out <- boxplot.stats(irv_total)$out
  out_ind_irv <- which(irv_total %in% c(out))
  # out_ind_irv
  
  # The longest consecutive string of identical responses given by a person.
  careless_long <- longstring(df, avg = FALSE)
  # boxplot(careless_long)
  out <- boxplot.stats(careless_long)$out
  out_ind_long <- which(careless_long %in% c(out))
  # out_ind_long
  
  outliers <- c(
    out_ind_mahad, out_pers_tot_cor, out_ind_psychsyn, out_ind_psychant, 
    out_ind_irv, out_ind_long
  )
  return(outliers)
}



#' simple function for computing z scores
standardize <- function(x){
  (x - mean(x))/sd(x)
}


#' function for fitting the 4 mediators model (coping strategies, social support,
#' positive self compassion, negative self compassion) for the standardized data.
#' The function returns a tibble with the direct effect, the 4 indirect effects,
#' and the total effect.
#' Before running the function, it is necessary to redefine the variable Xz, which
#' corresponds to one of the five personality factors.
#' 
fit_model_z <- function(d) {
  
  y_model  <- bf(Yz   ~ 1 + Xz + M_copz + M_sosz + M_pscz + M_nscz, family = skew_normal())
  m1_model <- bf(M_copz ~ 1 + Xz) # coping
  m2_model <- bf(M_sosz ~ 1 + Xz, family = skew_normal()) # social support
  m3_model <- bf(M_pscz ~ 1 + Xz) # positive SC
  m4_model <- bf(M_nscz ~ 1 + Xz, family = skew_normal()) # negative SC
  
  fit <-
    brm(data = d, 
        y_model + m1_model + m2_model + m3_model + m4_model + set_rescor(FALSE),
        chains = 4, 
        cores = 4,
        inits = 0
    )
  # fixef(fit_z) %>% round(digits = 3)
  # bayes_R2(fit)
  
  post <- posterior_samples(fit)
  
  # Computing the indirect effect with multiplication
  post <-
    post %>% 
    mutate(
      direct_eff = b_Yz_Xz,
      ind_eff_M_sos = b_Msosz_Xz * b_Yz_M_sosz,
      ind_eff_M_cop = b_Mcopz_Xz * b_Yz_M_copz,
      ind_eff_M_psc = b_Mpscz_Xz * b_Yz_M_pscz,
      ind_eff_M_nsc = b_Mnscz_Xz * b_Yz_M_nscz,
      indirect_eff = ind_eff_M_sos + ind_eff_M_cop + ind_eff_M_psc + ind_eff_M_nsc,
      total_eff = direct_eff + indirect_eff
    )
  
  output <- post %>% 
    select(
      direct_eff, 
      ind_eff_M_sos, 
      ind_eff_M_cop, 
      ind_eff_M_psc, 
      ind_eff_M_nsc, 
      indirect_eff,
      total_eff
    ) %>% 
    gather() %>%
    group_by(key) %>% 
    summarize(
      mean = mean(value), 
      ll = quantile(value, probs = .025),
      ul = quantile(value, probs = .975)
    ) %>% 
    mutate_if(is_double, round, digits = 3)
  
  output
  
}


# mediation model with 1 mediating variable

mediation_an_1 <- function(d) {
  
  set.seed(1234)
  
  d <- d %>% 
    dplyr::rename(
      X = x,
      Y = y,
      M = m
    )
  
  mediation_model <- ' 
    # direct effect
    Y ~ c*X
    # mediator
    M ~ a*X
    Y ~ b*M
    # indirect effect (a*b)
    ab := a*b
    # total effect
    total := c + (a*b)
  '
  
  fit <- sem(mediation_model, data = d)
  fit
}


# Two mediators
mediation_an_2 <- function(d) {
  
  set.seed(1234)
  
  d <- d %>% 
    dplyr::rename(
      X  = x,
      M1 = m1,
      M2 = m2,
      Y  = y
    )
  
  mediation_model <- ' 
   # direct effect
     Y ~ c*X
   # mediator
     M1 ~ a*X
     Y ~ b*M1
     M2 ~ d*X
     Y ~ e*M2
   # indirect effect (a*b)
     ab := a*b 
     de := d*e
     abde := a*b + d*e
   # total effect
     total := c + (a*b) + (d*e)
  '
  
  fit <- sem(mediation_model, data = d)
  fit
}





center_rename_yxm <- function(y, x, m) {
  
  d <- tibble(y, x, m) %>% 
    dplyr::mutate(
      xc = x - mean(x, na.rm = TRUE),
      yc = y - mean(y, na.rm = TRUE),
      mc = m - mean(m, na.rm = TRUE)
    ) %>% 
    dplyr::rename(
      X = xc,
      Y = yc,
      M = mc
    ) %>% 
    dplyr::select(X, Y, M)
  d
}



# get the medians and the 95% HDIs for the 3 variables in d

get_correlations <- function(d) {
  
  mod <- 
    brm(
      data = d, 
      family = gaussian,
      mvbind(X, Y, M) ~ 1,
      chains = 4, 
      cores = 4
    )
  
  foo <- posterior_samples(mod) %>% 
    select(rescor__X__Y, rescor__X__M, rescor__Y__M) 
  
  lluu <- as.numeric(HDInterval::hdi(foo$rescor__X__Y, credMass = 0.95))
  r_xy <- c(lluu[1], median(foo$rescor__X__Y), lluu[2])
  rm(lluu)
  
  lluu <- as.numeric(HDInterval::hdi(foo$rescor__X__M, credMass = 0.95))
  r_xm <- c(lluu[1], median(foo$rescor__X__M), lluu[2])
  rm(lluu)
  
  lluu <- as.numeric(HDInterval::hdi(foo$rescor__Y__M, credMass = 0.95))
  r_ym <- c(lluu[1], median(foo$rescor__Y__M), lluu[2])
  
  list(r_xy= r_xy, r_xm = r_xm, r_ym = r_ym)
}








