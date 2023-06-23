
save_boxplot_fig <- function(
    all_items, 
    subscale_names, 
    x_axis_label, # ex., "NEO-FFI-60 Sub-scales"
    saved_file_name # ex., "neo_ffi_all_subscales.pdf"
  ) {
  
  suppressPackageStartupMessages(library("here"))
  suppressPackageStartupMessages(library("tidyverse")) 
  suppressPackageStartupMessages(library("patchwork"))
  suppressPackageStartupMessages(library("ggthemes"))
  suppressPackageStartupMessages(library("ggokabeito"))
  suppressPackageStartupMessages(library("gghalves")) 
  suppressPackageStartupMessages(library("ggbeeswarm"))
  
  # Select variables.
  df <- all_items %>% 
    dplyr::select(all_of(subscale_names))
  
  # Draw plot. 
  long_df <- df %>%
    pivot_longer(!is_rescue_worker, names_to = "vars", values_to = "values")
  
  if (x_axis_label == "COPE-NVI Sub-scales") {
    long_df$vars <- long_df$vars %>% 
      dplyr::recode_factor(
        avoiding_strategies = "avoiding strategies", 
        positive_attitude = "positive attitude",
        problem_orientation = "problem orientation",
        social_support = "social support",
        transcendent_orientation = "transcendent orientation"
      )
  }
  
  if (x_axis_label == "SCS Sub-scales") {
    long_df$vars <- long_df$vars %>% 
      dplyr::recode_factor(
        self_kindness = "self kindness", 
        self_judgment = "self judgment",
        common_humanity = "common humanity",
        isolation = "isolation",
        mindfulness = "mindfulness",
        over_identification = "over identification"
      )
  }
  
  if (x_axis_label == "MSPSS Sub-scales") {
    long_df$vars <- long_df$vars %>% 
      dplyr::recode_factor(
        family = "family", 
        friends = "friends",
        common_humanity = "common humanity",
        significant_other = "significant other",
        mindfulness = "mindfulness",
        over_identification = "over identification"
      )
  }
  
  if (x_axis_label == "PTGI Sub-scales") {
    long_df$vars <- long_df$vars %>% 
      dplyr::recode_factor(
        relating_to_others = "relating to others", 
        new_possibilities = "new possibilities",
        personal_strength = "personal strength",
        spirituality = "spirituality",
        appreciation_of_life = "appreciation of life"
      )
  }
  
  myplot <- long_df |> 
  ggplot(aes(x = fct_inorder(vars), y = values, fill = is_rescue_worker)) +
    introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
    geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
    stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
                 position = position_dodge(.175)) +
    #scale_x_discrete(name = "Condition", labels = c("Non-word", "Word")) +
    scale_y_continuous(name = " ") +
    # scale_fill_brewer(palette = "Dark2", name = "Rescue worker") +
    scale_fill_okabe_ito() +
    scale_color_okabe_ito() +
    papaja::theme_apa() +
    coord_flip() +
    labs(
      x = x_axis_label,
      y = "Values"
    ) + 
    guides(fill=guide_legend(title="Rescue worker")) 
  
  

  # myplot <- 
  #   long_df %>% 
  #   ggplot(aes(x = fct_inorder(vars), y = values)) +
  #   geom_half_point(aes(color = is_rescue_worker), 
  #                   transformation = position_quasirandom(width = 0.1),
  #                   side = "l", size = 0.5, alpha = 0.5) +
  #   gghalves::geom_half_boxplot(aes(fill = is_rescue_worker), side = "r") + 
  #   scale_fill_okabe_ito() +
  #   scale_color_okabe_ito() +
  #   guides(color = "none", fill = "none") +
  #   papaja::theme_apa() +
  #   coord_flip() +
  #   labs(
  #     x = x_axis_label,
  #     y = "Values"
  #   ) + 
  #   guides(fill=guide_legend(title="Rescue-workers\n sample")) 
  
  # Save
  ggsave(here::here("reports", "suppl_mat", "suppl_figs", saved_file_name))
}


save_pdf_6_scales <- function(all_items) {
  
  # Generate figure with half-boxplots of the NEO-FFI-60 data.
  x_axis_label <- "NEO-FFI-60 Sub-scales"
  subscale_names <- c(
    "neuroticism", "extraversion", "openness", "agreeableness", 
    "conscientiousness", "is_rescue_worker"
  )
  save_boxplot_fig(
    all_items,
    scales_names,
    x_axis_label,
    "neoffi60_all_subscales.pdf"
  )

  # Generate figure with half-boxplots of the COPE-NVI data.
  x_axis_label <- "COPE-NVI Sub-scales"
  scales_names <- c(
    "social_support", "avoiding_strategies", "positive_attitude", 
    "problem_orientation", "transcendent_orientation", "is_rescue_worker"
  )
  save_boxplot_fig(
    all_items,
    scales_names,
    "COPE-NVI Sub-scales",
    "copenvi_all_subscales.pdf"
  )

  # Generate figure with half-boxplots of the PTGI data.
  x_axis_label <- "IES-R Sub-scales"
  scales_names <- c("avoiding", "intrusivity", "hyperarousal", "is_rescue_worker")
  save_boxplot_fig(
    all_items,
    scales_names,
    "IES-R Sub-scales",
    "ies_all_subscales.pdf"
  )

  # Generate figure with half-boxplots of the SCS data.
  x_axis_label <- "SCS Sub-scales"
  scales_names <- c(
    "self_kindness", "self_judgment", "common_humanity", "isolation", 
    "mindfulness", "over_identification", "is_rescue_worker"
  )
  save_boxplot_fig(
    all_items,
    scales_names,
    "SCS Sub-scales",
    "scs_all_subscales.pdf"
  )

  # Generate figure with half-boxplots of the MSPSS data.
  x_axis_label <- "MSPSS Sub-scales"
  scales_names <- c(
    "family", "friends", "significant_other", "is_rescue_worker"
  )
  save_boxplot_fig(
    all_items,
    scales_names,
    "MSPSS Sub-scales",
    "mspss_all_subscales.pdf"
  )

  # Generate figure with half-boxplots of the PTGI data.
  x_axis_label <- "PTGI Sub-scales"
  scales_names <- c(
    "relating_to_others", "new_possibilities", "personal_strength", 
    "spirituality", "appreciation_of_life", "is_rescue_worker"
  )
  save_boxplot_fig(
    all_items,
    scales_names,
    "PTGI Sub-scales",
    "ptgi_all_subscales.pdf"
  )
  
}

# eof ---



