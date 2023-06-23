plot_job_qualification <- function(all_items) {
  
  library("bayesplot")
  theme_set(bayesplot::theme_default())
  
  rw_df <- all_items |> 
    dplyr::filter(group == "rescue_workers")
  rw_df$job_qualification <- factor(rw_df$job_qualification)
  
  # Plot SJ
  m_sj <- brm(
    sj ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_sj)
  # summary(m_sj)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_sj, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(sj = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(sj) %>% 
    mutate(density = dnorm(sj, mu, sigma))
  
  p_sj <- densities %>% 
    ggplot(aes(x = sj, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "SJ Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_text(hjust = 0),
          axis.ticks.y = element_blank())
  # p_sj
  
  # Plot IS
  m_is <- brm(
    is ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_is)
  # summary(m_is)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_is, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(is = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(is) %>% 
    mutate(density = dnorm(is, mu, sigma))
  
  p_is <- densities %>% 
    ggplot(aes(x = is, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "IS Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  # p_is
  
  # Plot OI
  m_oi <- brm(
    oi ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_is)
  # summary(m_is)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_oi, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(oi = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(oi) %>% 
    mutate(density = dnorm(oi, mu, sigma))
  
  p_oi <- densities %>% 
    ggplot(aes(x = oi, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "OI Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  # p_oi
  
  # Plot SK
  m_sk <- brm(
    sk ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_is)
  # summary(m_is)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_sk, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(sk = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(sk) %>% 
    mutate(density = dnorm(sk, mu, sigma))
  
  p_sk <- densities %>% 
    ggplot(aes(x = sk, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "SK Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_text(hjust = 0),
          axis.ticks.y = element_blank())
  # p_sk
  
  # Plot CH
  m_ch <- brm(
    ch ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_ch)
  # summary(m_ch)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_ch, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(ch = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(ch) %>% 
    mutate(density = dnorm(ch, mu, sigma))
  
  p_ch <- densities %>% 
    ggplot(aes(x = ch, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "CH Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  # p_ch
  
  # Plot MI
  m_mi <- brm(
    mi ~ job_qualification,
    data = rw_df, 
    family = gaussian(), 
    backend = "cmdstanr"
  )
  # pp_check(m_is)
  # summary(m_is)
  
  densities <-
    rw_df %>% 
    distinct(job_qualification) %>% 
    add_epred_draws(m_mi, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))
  
  densities <-
    densities %>% 
    mutate(ll = qnorm(.025, mean = mu, sd = sigma),
           ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
    mutate(mi = map2(ll, ul, seq, length.out = 100)) %>% 
    unnest(mi) %>% 
    mutate(density = dnorm(mi, mu, sigma))
  
  p_mi <- densities %>% 
    ggplot(aes(x = mi, y = job_qualification)) +
    # here we make our density lines
    geom_ridgeline(aes(height = density, group = interaction(job_qualification, .draw)),
                   fill = NA,
                   size = 1/3, scale = 25) +
    # the original data with little jitter thrown in
    geom_jitter(data = rw_df,
                height = .04, alpha = 3/4) +
    # scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 26), 
    #                   expand = expansion(mult = c(0, 0.05))) +
    labs(title = "MI Data with Posterior Predictive Distribution", 
         y = NULL, x = NULL) +
    coord_cartesian(ylim = c(1.25, 5.85)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  # p_mi
  
  (p_sj + p_is + p_oi) / (p_sk + p_ch + p_mi)
  
  ggsave(
    here::here(
      "reports", "suppl_mat", "suppl_figs", "job_qualification.pdf"
    ),
    width = 35, height = 22.5, units = "cm"
  )
}
