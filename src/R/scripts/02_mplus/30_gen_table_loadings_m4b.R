# Script name: 30_gen_table_loadings_m4b.R
# Project: rescue-workers
# Script purpose: generate LaTeX code for loadings of model m4b
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Oct 27 14:16:20 2022
# Last Modified Date: Thu Oct 27 14:16:20 2022
# 
# Notes: 


runModels(paste0(the_dir, "/m4b.inp"))
model_results <- readModels(paste0(the_dir, "/m4b.out"), quiet = TRUE)
# summary(model_results)
# CFI = 0.85, TLI = 0.822, SRMR = 0.061 
# RMSEA = 0.12, 90% CI [0.116, 0.123], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

item_names <- lambdas[1:26, 2]

Items <- c(
  "SCSK05",  "SCSK12", "SCSK19", "SCSK23", "SCSK26", 
  "SCSJ01",  "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCCH03",  "SCCH07", "SCCH10", "SCCH15", 
  "SCIS04",  "SCIS13", "SCIS18", "SCIS25",
  "SCMI09",  "SCMI14", "SCMI17", "SCMI22", 
  "SCOI02",  "SCOI06", "SCOI20", "SCOI24"
)

SC <- round(lambdas[1:26, 3], 2)
SK <- round(lambdas[27:52, 3], 2)
SJ <- round(lambdas[53:78, 3], 2)
CH <- round(lambdas[79:104, 3], 2)
IS <- round(lambdas[105:130, 3], 2)
MI <- round(lambdas[131:156, 3], 2)
OI <- round(lambdas[157:182, 3], 2)

new_order <- c(
  "SCSK05", "SCSK12", "SCSK19", "SCSK23", "SCSK26",
  "SCMI09", "SCMI14", "SCMI17", "SCMI22", 
  "SCCH03", "SCCH07", "SCCH10", "SCCH15", 
  "SCSJ01", "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCIS04", "SCIS13", "SCIS18", "SCIS25",
  "SCOI02", "SCOI06", "SCOI20", "SCOI24"
)

# tbl <- data.frame(Items, SC, SK, SJ, CH, IS, MI, OI) %>% 
tbl <- data.frame(Items, SC, SK, MI, CH, SJ, IS, OI) %>% 
  arrange(match(Items, new_order)) 

tbl$grp <- c(
  rep("Self-kindness", 5),
  rep("Mindfulness", 4),
  rep("Common Humanity", 4),
  rep("Self-judgment", 5),
  rep("Isolation", 4),
  rep("Over-identification", 4)
)

tab_2 <-
  tbl %>% 
  group_by(grp) %>% # respects grouping from dplyr
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 2,
      rows = 14:26
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 3,
      rows = 1:5 #SK
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 4,
      rows = 6:9 # MI
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 5,
      rows = 10:13 # CH
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 6,
      rows = 14:18 # SJ
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 7,
      rows = 19:22 # IS
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 8,
      rows = 23:26 # OI
    )
  ) 


tab_2

# Saving as tex file 
tab_2 %>%
  gtsave(
    "m4b_loadings.tex",
    path = here("src", "R", "scripts", "02_mplus")
  )
