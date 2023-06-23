
#' @details Perform a LPA with the careless responding indices. Adds a column 
#' to the input data.frame with FLAG_1 ("keep", "delete") to flag the 
#' participants belonging to the two classes characterized by extreme values on  
#' the careless responding indices.
#' 
#' @import nothing
#' @return data.frame
#' 
flag_careless_responding_LPA <- function(all_items) {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse"))
  library("forcats")
  library("readxl")
  library("mice")
  library("careless")
  library("tidyLPA")
  
  options(max.print = 999999)
  
  set.seed(12345)
  
  # # Read data.
  # all_items <- readRDS(
  #   file = here(
  #     "data", "processed", "all_items",
  #     "final_complete_rescue_workers_controls_data.Rds"
  #   )
  # )
  
  dat <- all_items %>%
    dplyr::select_if(is.numeric)
  
  # Remove age.
  dat$age <- NULL
  
  # Compute careless statistics indices.
  preds_mahad <- mahad(dat, plot = FALSE, flag = FALSE, confidence = 0.99)
  temp <- longstring(dat, avg = TRUE)
  longstr <- temp[, 1]
  avgstr <- temp[2]
  cm <- colMeans(dat)
  ptc <- apply(dat, 1, function(x) cor(x, cm))
  preds_psychsyn <- psychsyn(dat)
  # preds_psychant <- psychant(dat)
  # preds_psychant <- ifelse(is.na(preds_psychant), 0, preds_psychant)
  irv_total <- irv(dat)
  
  d <- cbind(preds_mahad, longstr, avgstr, ptc, preds_psychsyn, irv_total)
  
  # LPA analysis on careless responding measures.
  
  d %>%
    scale() %>%
    estimate_profiles(1:6) %>%
    compare_solutions(statistics = c("AIC", "BIC"))
  # Compare tidyLPA solutions:
  #   
  #   Model Classes AIC       BIC      
  # 1     1       19088.531 19148.784
  # 1     2       17856.940 17952.341
  # 1     3       17636.983 17767.532
  # 1     4       16671.732 16837.428
  # 1     5       16247.922 16448.766
  # 1     6       16082.208 16318.199
  # 
  # Best model according to AIC is Model 1 with 6 classes.
  # Best model according to BIC is Model 1 with 6 classes.
  # 
  # An analytic hierarchy process, based on the fit indices AIC, AWE, BIC, CLC, 
  # and KIC (Akogul & Erisoglu, 2017), suggests the best solution is Model 1 
  # with 6 classes.
  
  mod <- d %>%
    scale() %>%
    estimate_profiles(6)
  
  by_subj_vals <- get_data(mod)
  table(by_subj_vals$Class)
  # 1   2   3   4   5   6 
  # 37 183 200 559 126  15 
  
  m6 <- d %>%
    scale() %>%
    estimate_profiles(6)
  
  get_estimates(m6) %>%
    as.data.frame()
  #     Category      Parameter    Estimate         se             p Class Model Classes
  # 1      Means    preds_mahad -0.52349346 0.18238768  4.101813e-03     1     1       6
  # 2      Means        longstr  3.37331736 0.75510535  7.919573e-06     1     1       6
  # 3      Means         avgstr  2.34901726 2.39455265  3.266008e-01     1     1       6
  # 4      Means            ptc  0.33324705 0.18167975  6.661610e-02     1     1       6
  # 5      Means preds_psychsyn  1.11339408 0.12201466  7.168055e-20     1     1       6
  # 6      Means      irv_total  0.07330740 0.23143563  7.514328e-01     1     1       6
  # 7  Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     1     1       6
  # 8  Variances        longstr  0.53111656 0.05475454  3.016323e-22     1     1       6
  # 9  Variances         avgstr  0.78104453 0.43308350  7.131754e-02     1     1       6
  # 10 Variances            ptc  0.33727904 0.02690995  4.888076e-36     1     1       6
  # 11 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     1     1       6
  # 12 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     1     1       6
  # 13     Means    preds_mahad  0.84557679 0.07010659  1.690866e-33     2     1       6
  # 14     Means        longstr  0.05333746 0.17245836  7.571106e-01     2     1       6
  # 15     Means         avgstr  0.01553329 0.12547998  9.014807e-01     2     1       6
  # 16     Means            ptc -0.60989343 0.18215532  8.133668e-04     2     1       6
  # 17     Means preds_psychsyn  0.17297662 0.09210269  6.036963e-02     2     1       6
  # 18     Means      irv_total  0.79898625 0.09558449  6.327732e-17     2     1       6
  # 19 Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     2     1       6
  # 20 Variances        longstr  0.53111656 0.05475454  3.016323e-22     2     1       6
  # 21 Variances         avgstr  0.78104453 0.43308350  7.131754e-02     2     1       6
  # 22 Variances            ptc  0.33727904 0.02690995  4.888076e-36     2     1       6
  # 23 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     2     1       6
  # 24 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     2     1       6
  # 25     Means    preds_mahad -0.17062596 0.06103157  5.178746e-03     3     1       6
  # 26     Means        longstr -0.51229347 0.03924451  6.038459e-39     3     1       6
  # 27     Means         avgstr -0.28720816 0.06054982  2.102270e-06     3     1       6
  # 28     Means            ptc -0.22172810 0.09724310  2.259926e-02     3     1       6
  # 29     Means preds_psychsyn -0.77790541 0.09059693  8.970302e-18     3     1       6
  # 30     Means      irv_total -0.79794088 0.11922834  2.193242e-11     3     1       6
  # 31 Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     3     1       6
  # 32 Variances        longstr  0.53111656 0.05475454  3.016323e-22     3     1       6
  # 33 Variances         avgstr  0.78104453 0.43308350  7.131754e-02     3     1       6
  # 34 Variances            ptc  0.33727904 0.02690995  4.888076e-36     3     1       6
  # 35 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     3     1       6
  # 36 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     3     1       6
  # 37     Means    preds_mahad -0.59662965 0.03218623  1.042427e-76     4     1       6
  # 38     Means        longstr  0.05035210 0.06878085  4.641284e-01     4     1       6
  # 39     Means         avgstr -0.02485096 0.04487287  5.797102e-01     4     1       6
  # 40     Means            ptc  0.68644601 0.02367835 8.682121e-185     4     1       6
  # 41     Means preds_psychsyn  0.56780468 0.03802195  1.992053e-50     4     1       6
  # 42     Means      irv_total -0.10292804 0.06036021  8.815154e-02     4     1       6
  # 43 Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     4     1       6
  # 44 Variances        longstr  0.53111656 0.05475454  3.016323e-22     4     1       6
  # 45 Variances         avgstr  0.78104453 0.43308350  7.131754e-02     4     1       6
  # 46 Variances            ptc  0.33727904 0.02690995  4.888076e-36     4     1       6
  # 47 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     4     1       6
  # 48 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     4     1       6
  # 49     Means    preds_mahad  1.25099510 0.07556962  1.492956e-61     5     1       6
  # 50     Means        longstr -0.47084471 0.04609331  1.698203e-24     5     1       6
  # 51     Means         avgstr -0.24983491 0.05723640  1.271431e-05     5     1       6
  # 52     Means            ptc -1.59235606 0.10375065  3.657596e-53     5     1       6
  # 53     Means preds_psychsyn -1.43214156 0.18113450  2.647067e-15     5     1       6
  # 54     Means      irv_total  0.27550225 0.07064311  9.622565e-05     5     1       6
  # 55 Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     5     1       6
  # 56 Variances        longstr  0.53111656 0.05475454  3.016323e-22     5     1       6
  # 57 Variances         avgstr  0.78104453 0.43308350  7.131754e-02     5     1       6
  # 58 Variances            ptc  0.33727904 0.02690995  4.888076e-36     5     1       6
  # 59 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     5     1       6
  # 60 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     5     1       6
  # 61     Means    preds_mahad  4.70927213 0.21326430 4.722648e-108     6     1       6
  # 62     Means        longstr -0.35512739 0.14682658  1.557668e-02     6     1       6
  # 63     Means         avgstr  0.64915828 0.17199344  1.604447e-04     6     1       6
  # 64     Means            ptc -2.26487023 0.12332384  2.494057e-75     6     1       6
  # 65     Means preds_psychsyn -3.46106086 0.27795790  1.368211e-35     6     1       6
  # 66     Means      irv_total  2.15413454 0.09097900 6.183441e-124     6     1       6
  # 67 Variances    preds_mahad  0.21982412 0.01415080  2.029885e-54     6     1       6
  # 68 Variances        longstr  0.53111656 0.05475454  3.016323e-22     6     1       6
  # 69 Variances         avgstr  0.78104453 0.43308350  7.131754e-02     6     1       6
  # 70 Variances            ptc  0.33727904 0.02690995  4.888076e-36     6     1       6
  # 71 Variances preds_psychsyn  0.29276228 0.03396701  6.752997e-18     6     1       6
  # 72 Variances      irv_total  0.70384223 0.04499329  3.692674e-55     6     1       6
  
  (15+37) / nrow(dat)
  # [1] 0.04642857
  
  by_subj_vals$index <- 1:nrow(d)
  by_subj_vals$FLAG_1 <- ifelse(
    by_subj_vals$Class == 1 | by_subj_vals$Class == 6, "delete", "keep"
  )
  
  all_items$FLAG_1 <- by_subj_vals$FLAG_1
  
  all_items
  
}