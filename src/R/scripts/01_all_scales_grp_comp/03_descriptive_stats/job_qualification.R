
d2$job_qualification <- recode_factor(
  d2$job_qualification, 
  Infermiere = "Soccorritore", 
  Medico = "Soccorritore")


d2 |> 
  group_by(grp, job_qualification) |> 
  summarise(
    avg_cs_p = mean(pos_sc),
    avg_cs_n = mean(neg_sc),
    avg_sk = mean(sk),
    avg_ch = mean(ch),
    avg_mi = mean(mi),
    avg_sj = mean(sj),
    avg_is = mean(is),
    avg_oi = mean(oi),
    n = n()
  )

rw_df <- d2 |> 
  dplyr::filter(grp == 0)

rw_df$job_qualification <- relevel(rw_df$job_qualification, "Autista")

m_sj <- lm(sj ~ job_qualification, rw_df)
car::Anova(m_sj)
summary(m_sj)

m_is <- lm(is ~ job_qualification, rw_df)
car::Anova(m_is)
summary(m_is)

m_oi <- lm(oi ~ job_qualification, rw_df)
car::Anova(m_oi)
summary(m_oi)


m_oi <- brm(oi ~ job_qualification,
           data = rw_df, family = skew_normal(), backend = "cmdstanr")
summary(m_oi)
pp_check(m_oi)
