
# https://mateuspsi.github.io/esemComp/index.html

library(esemComp)

rw_df <- rw_df |> 
  ungroup()

ov <- c(
  "neuroticism", "extraversion", "openness", "agreeableness",
    "conscientiousness", 
    "active_coping", "avoidance_coping", "soc_emo_coping",
    "family", "friends", "significant_other",
    "avoiding", "intrusivity", "hyperarousal"
)

fa_df <- rw_df |> 
  dplyr::select(all_of(ov))

# list with mapping between factors and items
main_loadings_list <- list(bigfive = c(1:5),
                           coping = c(6:8),
                           mspss = c(9:11),
                           ies = c(12:14))
target_rot <- make_target(nitems = 14, mainloadings = main_loadings_list)
target_rot


#make exploratory analysis with geomin rotation
geomin_efa <- esem_efa(fa_df, 4)

referents_list <- list(ies = "ie", five = "fi", mspss = "ps", cope = "co")

model_syntax <- syntax_composer(geomin_efa, referents_list)
writeLines(model_syntax)

# esem-within-cfa
esem_w_cfa <- lavaan::cfa(model_syntax, data = fa_df, std.lv = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE)
