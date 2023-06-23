
#' The get_cohen_d() function runs a brm() model and returns the 95% credibility
#' interval, and the map, for the effect size (Cohen's d) for the posterior 
#' comparison between the groups.
get_cohen_d <- function(bform, var_name, data) {
  
  library("brms")
  library("cmdstanr")
  library("HDInterval")
  library("broom.mixed")
  library("rlang")
  
  fit <-
    brm(data = data,
        family = gaussian,
        bform,
        prior = c(prior(normal(0, 1 * 5), class = Intercept),
                  prior(normal(0, 1 * 5), class = b),
                  prior(cauchy(0, 1), class = sigma)),
        iter = 3000, 
        warmup = 1000, 
        chains = 4, 
        cores = 6,
        seed = 12345,
        backend = "cmdstanr"
    )
  
  tidy(fit, effects = "fixed", conf.method="HPDinterval")
  
  # Get posterior distribution.
  delta_t <-
    # extracting posterior samples from bmod5
    posterior_samples(fit, pars = c("^b_", "sigma")) %>%
    # taking the square of each variance component
    mutate_at(.vars = 3, .funs = funs(.^2) ) %>%
    # dividing the slope estimate by the square root of the sum of
    # all variance components
    mutate(delta = {{var_name}} / sqrt(rowSums(.[3]) ) )
  
  # Cohen's d,
  out <- HDInterval::hdi(delta_t$delta)
  
  c(out[1], map = median(delta_t$delta), out[2]) %>% 
    round(3)
} 



