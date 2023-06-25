#' Script name: 101_run_mplus_lpa_1.R
#' Project: self-compassion
#' Script purpose: Two-Profile Model with BCH Covariate Analysis
#' @author: Corrado Caudek <corrado.caudek@unifi.it>
#' Date Created: Sun Jun 25 08:33:29 2023
#' Last Modified Date: Sun Jun 25 08:33:29 2023
#' 
#' Notes: 
#' Covariate analysis is performed by following the procedure of
#' https://doi.org/10.1177/0165025419881


library("here")
library("tidyverse")
library("MplusAutomation")
library("gt")
library("glue")
library("kableExtra")
library("misty")

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)

options(mc.cores = 4)


# Save data for Mplus -----------------------------------------------

# Read the complete data.
d <- rio::import(
  file = here("data", "processed", "lpa", "lpa_final.csv")
)
nrow(d)

# Converts an R data.frame into a tab-delimited file (without header) to be 
# used in an Mplus input file. 
MplusAutomation::prepareMplusData(
  d, 
  file = here::here(
    "src", "R", "scripts", "02_mplus", "mplus_models", "lpa", 
    "Mplus_LPA_Final.dat"
  )
)

# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here(
  "src", "R", "scripts", "02_mplus", "mplus_models", "lpa"
)


# BCH approach ------------------------------------------------------

#' Save the bch weights based upon the Classification Probabilities for the 
#' Most Likely Latent Class Membership by Latent Class. These are used in the 
#' next modeling step to specify the profiles so that they are not affected by 
#' the inclusion of the covariates in the model.
runModels(paste0(the_dir, "/step2.inp"), showOutput = TRUE)

runModels(paste0(the_dir, "/step3.inp"), showOutput = TRUE)



