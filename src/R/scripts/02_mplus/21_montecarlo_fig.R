# Script name: 21_montecarlo_fig.R
# Project: self-compassion
# Script purpose: generate figure for monte carlo sample size.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Sat Nov 12 09:51:19 2022
# Last Modified Date: Sat Nov 12 09:51:19 2022
# 
# Notes: 


library("tidyverse")
library("here")

d <- tribble(
  ~coef, ~sample_size, ~power,
  "P",       100,       .232,
  "P",       200,       .481,
  "P",       300,       .667,
  "P",       400,       .794, 
  "P",       500,       .880,
  "P",       600,       .930,
  "P",       700,       .960,     
  "P",       800,       .979,
  "P",       900,       .989,
  "P",      1000,       .993,
  "N",       100,       .825,
  "N",       200,       .986,
  "N",       300,       .999,
  "N",       400,      1.000, 
  "N",       500,      1.000, 
  "N",       600,      1.000, 
  "N",       700,      1.000,     
  "N",       800,      1.000, 
  "N",       900,      1.000, 
  "N",      1000,      1.000
)


d |> 
  ggplot(aes(x=sample_size, y=power, group=coef)) +
  geom_line(aes(linetype=coef)) +
  geom_point(aes(shape=coef, color=coef), size=4) +
  labs(title="Plot of power by sample size", x="Sample size", y = "Power")
