
scs_pos_items <- all_items |> 
  dplyr::select(
    scs_5, scs_12, scs_19, scs_23, scs_26,
    scs_3, scs_7, scs_10, scs_15,
    scs_9, scs_14, scs_17, scs_22
  )
all_items$pos_sc <- rowSums(scs_pos_items)

scs_neg_items <- all_items |> 
  dplyr::select(
    scs_1, scs_8, scs_11, scs_16, scs_21,
    scs_4, scs_13, scs_18, scs_25,
    scs_2, scs_6, scs_20, scs_24
  )
all_items$neg_sc <- rowSums(scs_neg_items)

# SCS TS
all_items$ts_sc <- all_items$pos_sc + all_items$neg_sc

# SCS subscales.
sk_df <- all_items |> 
  dplyr::select(
    scs_5, scs_12, scs_19, scs_23, scs_26
  )
all_items$sk <- rowSums(sk_df)

ch_df <- all_items |> 
  dplyr::select(
    scs_3, scs_7, scs_10, scs_15
  )
all_items$ch <- rowSums(ch_df)

mi_df <- all_items |> 
  dplyr::select(
    scs_9, scs_14, scs_17, scs_22
  )
all_items$mi <- rowSums(mi_df)

sj_df <- all_items |> 
  dplyr::select(
    scs_1, scs_8, scs_11, scs_16, scs_21
  )
all_items$sj <- rowSums(sj_df)

is_df <- all_items |> 
  dplyr::select(
    scs_4, scs_13, scs_18, scs_25
  )
all_items$is <- rowSums(is_df)

oi_df <- all_items |> 
  dplyr::select(
    scs_2, scs_6, scs_20, scs_24
  )
all_items$oi <- rowSums(oi_df)

all_items$grp <- ifelse(as.character(all_items$group) == "rescue_workers", 0, 1)


all_items |> 
  group_by(job_qualification) |> 
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

rw_df <- all_items |> 
  dplyr::filter(job_qualification != "Non rescue-worker")

rw_df$job_qualification <- factor(rw_df$job_qualification)
contrasts(rw_df$job_qualification)


# SJ ----------------------------------------------------------------------

m_sj <- brm(
  sj ~ job_qualification,
  data = rw_df, 
  family = gaussian(), 
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  backend = "cmdstanr"
)
pp_check(m_sj)
summary(m_sj)

hypothesis(m_sj, "job_qualificationTeamleader = 0")
# beta = 1.55
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     1.55      0.46     0.66     2.46         NA        NA    *
     
hypothesis(m_sj, "job_qualificationRescueworker = 0")
# beta = 1.62
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     1.62      0.46     0.75     2.52         NA        NA    *


# IS ----------------------------------------------------------------------

m_is <- brm(
  is ~ job_qualification,
  data = rw_df, 
  family = skew_normal(),
  backend = "cmdstanr"
)
pp_check(m_is)
summary(m_is)

hypothesis(m_is, "job_qualificationTeamleader = 0")
# beta = 0.38
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     0.38      0.25    -0.08     0.89         NA        NA     
   
hypothesis(m_is, "job_qualificationRescueworker = 0")
# beta = 1.10
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0      1.1       0.3     0.55     1.73         NA        NA    *
 

# OI ----------------------------------------------------------------------

m_oi <- brm(
  oi ~ job_qualification,
  data = rw_df, 
  family = skew_normal(),
  backend = "cmdstanr"
)
pp_check(m_oi)
summary(m_oi)

hypothesis(m_oi, "job_qualificationTeamleader = 0")
# beta = 0.41
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     0.41      0.23    -0.02     0.89         NA        NA     
   
hypothesis(m_oi, "job_qualificationRescueworker = 0")
# beta = 0.86
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     0.86      0.28     0.36     1.44         NA        NA    *


# SK ----------------------------------------------------------------------

m_sk <- brm(
  sk ~ job_qualification,
  data = rw_df, 
  family = gaussian(),
  backend = "cmdstanr"
)
pp_check(m_sk)
summary(m_sk)

hypothesis(m_sk, "job_qualificationTeamleader = 0")
# beta = -0.22
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0    -0.22      0.42    -1.05     0.63         NA        NA     

hypothesis(m_sk, "job_qualificationRescueworker = 0")
# beta = -0.29
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0    -0.29      0.42    -1.12     0.53         NA        NA    


# CH ----------------------------------------------------------------------

m_ch <- brm(
  ch ~ job_qualification,
  data = rw_df, 
  family = gaussian(),
  backend = "cmdstanr"
)
pp_check(m_ch)
summary(m_ch)

hypothesis(m_ch, "job_qualificationTeamleader = 0")
# beta = 0.58
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     0.58      0.33    -0.08     1.21         NA        NA         

hypothesis(m_ch, "job_qualificationRescueworker = 0")
# beta = 0.51
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0     0.51      0.33    -0.13     1.14         NA        NA     
   

# MI ----------------------------------------------------------------------

m_mi <- brm(
  mi ~ job_qualification,
  data = rw_df, 
  family = gaussian(),
  backend = "cmdstanr"
)
pp_check(m_mi)
summary(m_mi)

hypothesis(m_mi, "job_qualificationTeamleader = 0")
# beta = -0.47
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0    -0.47      0.31    -1.07     0.12         NA        NA             

hypothesis(m_mi, "job_qualificationRescueworker = 0")
# beta = -0.61
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (job_qualificatio... = 0    -0.61      0.31    -1.23    -0.02         NA        NA    *


# Raw data density plot.
# rw_df %>% 
#   group_by(job_qualification) %>% 
#   mutate(group_mean = mean(sj)) %>% 
#   ungroup() %>% 
#   mutate(job_qualification = fct_reorder(job_qualification, group_mean)) %>% 
#   ggplot(aes(x = sj, y = job_qualification, fill = group_mean)) +
#   geom_density_ridges(scale = 3/2, size = .2) +
#   # scale_fill_gradient(low = pp[4], high = pp[2]) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
#   scale_y_discrete(NULL, expand = expansion(mult = c(0, 0.4))) +
#   theme(axis.text.y = element_text(hjust = 0),
#         axis.ticks.y = element_blank(),
#         legend.position = "none")


# Save pdf figure with posterior estimates of the means of the three groups, 
# for each SCS subscale.
plot_job_qualification(rw_df)

