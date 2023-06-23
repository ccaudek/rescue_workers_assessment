# Script name: 01_get_data_for_mplus.R
# Project: self-compassion
# Script purpose: get data for MPLUS.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Oct  5 06:33:40 2022
# Last Modified Date: Wed Oct  5 06:33:40 2022
# 
# Notes: 

library("here")             # here function
library("tidyverse")        # ggplot, dplyr, and friends
library("MplusAutomation")  # prepareMplusData function
library("misty")

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6  # Use 6 cores
)

source(here::here("src", "R", "functions", "funs_add_neoffi60_subscales.R"))


# Get data ----------------------------------------------------------------

# Read the cleaned data.
d <- readRDS(
  file = here("data", "processed", "all_items", "self_comp_data.Rds")
)
nrow(d)

# Add NEO-FFI-60 subscales.
all_items <- add_neoffi60_subscales(d)

all_items |> 
  group_by(group) |> 
  summarise(
    n = n()
  )

# Add grp.
all_items$grp <- ifelse(
  all_items$group == "rescue_workers", "rescue_workers", "community_sample"
) |> 
  factor()

# Get data of the RW group only.
rw_df <- all_items |> 
  dplyr::filter(group == "rescue_workers")



# Save SCS data for Mplus -------------------------------------------------

scs_df <- rw_df |> 
  dplyr::select(
    # SCS
    all_of(contains("scs_"))
  )

# write.table(
#   scs_df, 
#   file = here::here(
#     "src", "R", "scripts", "02_mplus", "mplus_data", "scs_data", "scsdata.dat"
#   ), 
#   sep = " ", 
#   quote = FALSE, 
#   row.names = FALSE,
#   col.names = FALSE
# )

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
prepareMplusData(
  scs_df, 
  file = here::here(
    "src", "R", "scripts", "02_mplus", "mplus_data", "scs_data", "scsdata.dat"
  )
)


# Save SEM data for Mplus -------------------------------------------------

sem_df <- rw_df |> 
  dplyr::select(
    # SCS
    all_of(contains("scs_")), 
    # PTGI
    appreciation_of_life, new_possibilities, personal_strength, spirituality, 
    relating_to_others,
    # IES-R
    avoiding, intrusivity, hyperarousal,
    # COPE-NVI
    positive_attitude, problem_orientation,
    # MSPSS
    family, friends, significant_other,
    # NEO-FFI-60 Neuroticism
    negative_affect, self_reproach,
    # NEO-FFI-60 Extraversion
    positive_affect, sociability, activity
  )


# write.table(
#   sem_df, 
#   file = here::here(
#     "src", "R", "scripts", "02_mplus", "mplus_data", "sem_data", "semdata.dat"
#   ), 
#   sep = " ", 
#   quote = FALSE, 
#   row.names = FALSE,
#   col.names = FALSE
# )

sem_df$grp <- ifelse(sem_df$grp == "rescue_workers", 1, 0)

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
prepareMplusData(
  sem_df, 
  file = here::here(
    "src", "R", "scripts", "02_mplus", "mplus_data", "sem_data", "two_groups_data.dat"
  )
)


# eof ---


