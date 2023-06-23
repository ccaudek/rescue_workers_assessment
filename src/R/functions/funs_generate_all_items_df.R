#' The get_cohen_d() function runs a brm() model and returns the 95% credibility
#' interval, and the map, for the effect size (Cohen's d) for the posterior
#' comparison between the groups.
generate_all_items_df <- function() {
  # Read data
  d <- readRDS(
    file = here(
      "data", "processed", "all_items",
      "rescue_workers_final.Rds"
    )
  )

  # Add NEO-FFI-60 subscales.
  d1 <- add_neoffi60_subscales(d)

  # Correct IES-R scores.
  d2 <- correct_iesr_scores(d1)

  all_items <- d1 |>
    dplyr::filter(FLAG_1 == "keep")

  # Change factor job_qualification
  all_items$job_qualification <- recode_factor(
    all_items$job_qualification,
    Infermiere = "Soccorritore",
    Medico = "Soccorritore"
  )

  all_items$job_qualification <-
    forcats::fct_explicit_na(all_items$job_qualification, "Non soccorritore")

  all_items$job_qualification <- recode_factor(
    all_items$job_qualification,
    "Capo-squadra" = "team_leader",
    "Soccorritore" = "team_member",
    "Autista" = "driver",
    "Non soccorritore" = "non_rescue_worker"
  )
  all_items$job_qualification <- relevel(all_items$job_qualification, "driver")

  all_items <- all_items %>%
    mutate(
      rate_of_activity_num = recode(
        rate_of_activity,
        "Più di 1 turno a settimana"     = 2,
        "1 turno a settimana"            = 1,
        "1 turno ogni due settimane"     = 1 / 2,
        "1 turno al mese"                = 1 / 4,
        "Meno di 1 turno al mese"        = 1 / 10
      )
    )

  all_items <- all_items %>%
    mutate(
      last_training_num = recode(
        last_training,
        "Meno di 2 mesi fa"              = 1,
        "Più di 2 mesi fa"               = 3,
        "Meno di 6 mesi fa"              = 4,
        "Più di 6 mesi fa"               = 8,
        "Meno di 1 anno fa"              = 10,
        "Più di 1 anno fa"               = 18
      )
    )

  all_items <- all_items %>%
    mutate(
      education_num = recode(
        education,
        "Scuola media primaria"       = 8,
        "Diploma"                     = 13,
        "Laurea breve"                = 16,
        "Laurea magistrale"           = 18,
        "Dottorato"                   = 22
      )
    )


  # Compute SCS TS, SCS positive and SCS negative scores

  # scs_pos_items <- all_items |>
  #   dplyr::select(
  #     scs_5, scs_12, scs_19, scs_23, scs_26,
  #     scs_3, scs_7, scs_10, scs_15,
  #     scs_9, scs_14, scs_17, scs_22
  #   )
  # all_items$pos_sc <- rowSums(scs_pos_items)
  #
  # scs_neg_items <- all_items |>
  #   dplyr::select(
  #     scs_1, scs_8, scs_11, scs_16, scs_21,
  #     scs_4, scs_13, scs_18, scs_25,
  #     scs_2, scs_6, scs_20, scs_24
  #   )
  # all_items$neg_sc <- rowSums(scs_neg_items)

  # SCS TS
  all_items$scs_ts <- with(
    all_items,
    self_kindness + common_humanity + mindfulness +
      self_judgment + isolation + over_identification # reversed
  )
  #
  # # SCS subscales.
  # sk_df <- all_items |>
  #   dplyr::select(
  #     scs_5, scs_12, scs_19, scs_23, scs_26
  #   )
  # all_items$sk <- rowSums(sk_df)
  #
  # ch_df <- all_items |>
  #   dplyr::select(
  #     scs_3, scs_7, scs_10, scs_15
  #   )
  # all_items$ch <- rowSums(ch_df)
  #
  # mi_df <- all_items |>
  #   dplyr::select(
  #     scs_9, scs_14, scs_17, scs_22
  #   )
  # all_items$mi <- rowSums(mi_df)
  #
  # sj_df <- all_items |>
  #   dplyr::select(
  #     scs_1, scs_8, scs_11, scs_16, scs_21
  #   )
  # all_items$sj <- rowSums(sj_df)
  #
  # is_df <- all_items |>
  #   dplyr::select(
  #     scs_4, scs_13, scs_18, scs_25
  #   )
  # all_items$is <- rowSums(is_df)
  #
  # oi_df <- all_items |>
  #   dplyr::select(
  #     scs_2, scs_6, scs_20, scs_24
  #   )
  # all_items$oi <- rowSums(oi_df)


  # This coding system compares the mean of the dependent variable for a given
  # level to the overall mean of the dependent variable.
  contrasts(all_items$gender) <- contr.sum(2)

  all_items
}
