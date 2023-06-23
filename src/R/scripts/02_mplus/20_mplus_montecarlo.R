# Script name: 20_mplus_montecarlo.R
# Project: rescue-workers
# Script purpose: Monte Carlo simulation for deterining effect size of 
#  model m6.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Oct 27 10:09:32 2022
# Last Modified Date: Thu Oct 27 10:09:32 2022
# 
# Notes: 
# A tutorial of the Monte Carlo procedure for determining effect size is  
# provided in the book "Structural Equation Modeling: Applications using Mplus".


library("here")
library("tidyverse")
library("MplusAutomation")
library("gt")
library("glue")
library("kableExtra")
library("misty")
library("lavaan")
library("AICcmodavg")
library("nonnest2")
library("DiagrammeR")

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)

options(
  mc.cores = 6 # Use 6 cores
)

# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here(
  "src", "R", "scripts", "02_mplus", "mplus_models", "montecarlo"
)

# Run Mplus m6 model
runModels(paste0(the_dir, "/montecarlo_m6.inp"), showOutput = TRUE)


# eof ----



