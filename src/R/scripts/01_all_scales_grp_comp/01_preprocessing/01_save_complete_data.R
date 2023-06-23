# Script name: 01_save_complete_data.R
# Project: self compassion - one construct or two?
# Script purpose: creating an RDS file with all the items of the six scales,
#   for each of the three groups (rescue-workers, community sample, psicometria
#   students).
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Feb  4 10:55:28 2020
# Last Modified Date: Fri Oct  7 16:28:01 2022
#
# Notes: the missing items in the student sample have been imputed.
#  This is the final complete data set with all the information of the three
#  groups. It contains all items of all scales, and the demographic information.
#  here("data", "processed", "all_items", 
#  "final_complete_rescue_workers_controls_data.Rds")


# Set up ----

library("here")
suppressPackageStartupMessages(library("tidyverse"))
library("mice")

options(max.print = 999999)

set.seed(12345)

source(here("src", "R", "functions", "funs_get_items.R"))

# Rescue-workers sample.
rescue_workers_items <- get_items_rescue_workers() # 783
rescue_workers_items$group <- "rescue_workers"

# Correct id.
rescue_workers_items$id <- fct_recode(
  rescue_workers_items$id,
  "cc961796" = "Claudio Chiappone,9617,96",
  "AA911123" = "911",
  "DP910542"  = "DoraPineda910542",
  "PG987654" = "Pellegrini Giuliano",
  "az909876" = "az90",
  "AB108123" = "108",
  "mr515123" = "mariarossi515",
  "Mc999912"  = "Mc9999",
  "BG213859" = "BG, 2138,59",
  "BC150412" = "1504",
  "NO987654" = "NO",
  "DS755015" = "DSalvadori755015",
  "LZ196398" = "Lucio1963",
  "PCOE5597"  = "PCOELHOCARVALHO5597",
  "CRIB0028" = "0028cribre",
  "ABCD1673" = "1673",
  "GA230289" = "gaia230289",
  "MNBV1960" = "1960",
  "Edf98765" = "Edf",
  "NOQWA876" = "no",
  "CRI89123" = "Cri89",
  "AAB48542" = "48542",
  "GP420619" = "GiuseppinaPrina420619",
  "MR613728" = "MARIAROSABATTISTELLO613728",
  "ALB98123" = "albetira",
  "DOR12345" = "dora",
  "AB335834" = "AB3358341200"
)

rescue_workers_items$id <- rescue_workers_items$id |> 
  as.character()
rescue_workers_items$id[146] <- "QW0285356"
rescue_workers_items$id[613] <- "PZ4786283"
rescue_workers_items$id[782] <- "OGEW37573"
rescue_workers_items$id <- rescue_workers_items$id |> 
  as.factor()

rescue_workers_items$id <- paste("r", rescue_workers_items$id, sep = "_")
length(unique(rescue_workers_items$id))
duplicated(rescue_workers_items$id)

# Community sample.
community_sample_items <- get_items_community_sample() # 167
community_sample_items$group <- "community_sample"
backup_community_sample <- community_sample_items

# Correct id.
community_sample_items$id <- fct_recode(
  community_sample_items$id,
  "Kacu15049" = "Kacu 15049",
  "SC12345" = "SC",
  "Lmg598083" = "L m g 598083",
  "MarcoB123" = "Marco B.",
  "199601234" = "1996.0",
  "998001234" = "9980.0",
  "iicicsi12" = "Inserisci il codice identificativo come sopra indicato:",
  "Gr7123456" = "Gr7",
  "NN1234012" = "1234.0"
)

community_sample_items$id <- as.character(community_sample_items$id)
community_sample_items[63, 205] <- "LS27442"
community_sample_items[64, 205] <- "LS12345"
community_sample_items[41, 205] <- "AB5707457"
community_sample_items[102, 205] <- "WW045421"
community_sample_items[141, 205] <- "YE639185"

community_sample_items$id <- community_sample_items$id |>
  as.factor()
community_sample_items$id <- paste("c", community_sample_items$id, sep = "_")


# Student sample.
student_sample_items <- get_items_student_sample() # 170
student_sample_items$group <- "student_sample"
student_sample_items$education <- "Diploma"

student_sample_items$id <- paste("s", student_sample_items$id, sep = "_")

# Impute missing items of the student sample, by considering only the
# community sample.
two_groups_df <- full_join(
  community_sample_items, student_sample_items
)
length(unique(two_groups_df$id))
length(unique(community_sample_items$id))
length(unique(student_sample_items$id))

temp <- two_groups_df[, 1:201]
imp <- mice(temp, meth = "pmm", m = 1, maxit = 5)
temp1 <- complete(imp, 1)

two_groups_imputed_items <- bind_cols(temp1, two_groups_df[, 202:210])

# Join rescue workers sample with the other two groups.
all_items_df <- bind_rows(
  rescue_workers_items,
  two_groups_imputed_items
)

# Clean data.
all_items_df$gender <- factor(all_items_df$gender)
all_items_df$gender <- fct_recode(all_items_df$gender,
  "Femmina" = "Sesso"
)
# summary(all_items_df$gender)

all_items_df$is_rescue_worker <- factor(all_items_df$is_rescue_worker)
all_items_df$is_rescue_worker <-
  fct_recode(all_items_df$is_rescue_worker, "Si" = "Sì")
all_items_df$is_rescue_worker <-
  fct_recode(all_items_df$is_rescue_worker, "No" = "no")
# summary(all_items_df$is_rescue_worker)

all_items_df$group <- factor(all_items_df$group)
summary(all_items_df$group)


# Write data files to the "processed" directory:
saveRDS(all_items_df,
  file = here(
    "data", "processed", "all_items",
    "final_complete_rescue_workers_controls_data.Rds"
  )
)


# eof  ---
