# Script name: script_name
# Project: project
# Script purpose: script_purpose
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: last_modified_date
# 
# Notes: 

suppressPackageStartupMessages(library("here"))
suppressPackageStartupMessages(library("tidyverse")) # ggplot, dplyr, and friends
suppressPackageStartupMessages(library("misty"))
suppressPackageStartupMessages(library("lavaan"))
suppressPackageStartupMessages(library("semTools"))
suppressPackageStartupMessages(library("psych"))
suppressPackageStartupMessages(library("MBESS"))

source(here::here("src", "R", "functions", "funs_compute_subscales_scores.R"))



# Read data.

d <- rio::import(here::here("data", "processed", "self_compassion.csv"))

d <- compute_subscales_scores(d)

all_items <- d |> 
  dplyr::filter(FLAG_1 == "keep")
  # dplyr::filter(FLAG_1 == "keep" & group == "rescue_workers")


# COPE-NVI ----------------------------------------------------------------

# Positive attitude
positive_attitude <- tibble(
  all_items$cope_10, all_items$cope_22,
    all_items$cope_41, all_items$cope_49, all_items$cope_1, all_items$cope_29,
    all_items$cope_38, all_items$cope_59, all_items$cope_13, all_items$cope_21,
    all_items$cope_44, all_items$cope_54
  
)
reliability(positive_attitude)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.48  0.75      0.77 0.63  0.76   0.83      0.84      0.59    0.2  0.18      12

# Problem orientation
problem_orientation <- tibble(
  all_items$cope_5, all_items$cope_25, all_items$cope_47,
    all_items$cope_58, all_items$cope_19, all_items$cope_32, all_items$cope_39,
    all_items$cope_56, all_items$cope_15, all_items$cope_33, all_items$cope_42,
    all_items$cope_55
)
reliability(problem_orientation)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.66  0.84      0.85 0.84  0.87   0.97      0.88      0.74    0.3  0.27      12

cope <- cbind(positive_attitude, problem_orientation)
reliability(cope)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.38  0.86      0.87 0.64  0.73   0.88      0.91      0.72    0.2  0.19      24


# Social support
social_support <- tibble(
  all_items$cope_4, all_items$cope_14, all_items$cope_30, all_items$cope_45,
    all_items$cope_11, all_items$cope_23, all_items$cope_34, all_items$cope_52,
    all_items$cope_3, all_items$cope_17, all_items$cope_28, all_items$cope_46
)
  


m <- '
  ss  =~ cope_4 + cope_14 + cope_30 + cope_45 + cope_11 + 
         cope_23 + cope_34 + cope_52 + cope_3 + cope_17 + 
         cope_28 + cope_46
'

fit <- lavaan::cfa(m, data = all_items, ordered = TRUE, std.lv = TRUE)
semTools::reliability(fit)
# ss
# alpha     0.8020776
# alpha.ord 0.8428284
# omega     0.8311715
# omega2    0.8311715
# omega3    0.8635055
# avevar    0.3490973


# PTGI --------------------------------------------------------------------

# Relating to Others
relating_to_others <- tibble(
  i15=all_items$ptgi_15, i20=all_items$ptgi_20, i9=all_items$ptgi_9, 
  i21=all_items$ptgi_21, i8=all_items$ptgi_8, i16=all_items$ptgi_16, 
  i6=all_items$ptgi_6
)
reliability(relating_to_others)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items     0.8  0.91      0.92 0.98  0.99   0.99      0.91      0.86   0.59  0.59       7
  
# New Possibilities
new_possibilities <- tibble(
  i11=all_items$ptgi_11, i7=all_items$ptgi_7, i3=all_items$ptgi_3,
  i17=all_items$ptgi_17, i14=all_items$ptgi_14
)
reliability(new_possibilities)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.78  0.86      0.89 0.98  0.99   0.99      0.84      0.77   0.56  0.56       5

# Personal Strength
personal_strength <- tibble(
  i10=all_items$ptgi_10, i19=all_items$ptgi_19, i4=all_items$ptgi_4,
  i12=all_items$ptgi_12
)
reliability(personal_strength)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.03  0.82      0.83 0.98  0.98      1      0.83      0.81   0.54  0.53       4

# Appreciation of Life
appreciation_of_life <- tibble(
  i13=all_items$ptgi_13, i2=all_items$ptgi_2, i1=all_items$ptgi_1
)
reliability(appreciation_of_life)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.01  0.79      0.81 0.97  0.97      1      0.77       0.7   0.55  0.55       3

# Spiritual Enhancement
spirituality <- tibble(
  i5=all_items$ptgi_5, i18=all_items$ptgi_18
)
reliability(spirituality)
#           omega_h alpha omega.tot Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items       0  0.74      0.74   1     1      1      0.74      0.74   0.59  0.59       2

ptgi <- cbind(
  appreciation_of_life, new_possibilities, personal_strength,
  spirituality, relating_to_others
)
reliability(ptgi)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.74  0.95      0.95 0.92  0.94   0.98      0.97      0.89   0.47  0.47      21

m <- '
  rto =~ i15 + i20 + i9 + i21 + i8 + i16 + i6
  np =~ i11 + i7 + i3 + i17 + i14
  ps =~ i10 + i19 + i4 + i12
  aol =~ i13 + i2 + i1
  sp =~ i5 + i18
'

fit <- cfa(m, data = ptgi, ordered = TRUE)
summary(fit, standardized = TRUE)

semTools::compRelSEM(fit)
#   rto    np    ps   aol    sp 
# 0.920 0.886 0.850 0.827 0.773 

fit <- cfa(m, data = ptgi, ordered = FALSE)
semTools::compRelSEM(fit, return.total = TRUE)
# rto    np    ps   aol    sp total 
# 0.908 0.844 0.838 0.792 0.751 0.956 

# IES-R -------------------------------------------------------------------

# Avoidance
avoiding <- tibble(
  all_items$ies_5, all_items$ies_7, all_items$ies_8, all_items$ies_11,
    all_items$ies_12, all_items$ies_13, all_items$ies_17, all_items$ies_22
)
reliability(avoiding)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.57  0.78      0.82 0.78  0.81   0.96      0.83      0.67   0.31  0.35       8

# Intrusion
intrusivity <- tibble(
  all_items$ies_1, all_items$ies_2, all_items$ies_3, all_items$ies_6,
  all_items$ies_9, all_items$ies_14, all_items$ies_16, all_items$ies_20
)
reliability(intrusivity)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.83   0.9      0.91 0.97  0.97      1      0.92      0.87   0.52  0.54       8

# Hyperarousal
hyperarousal <- tibble(
  all_items$ies_4, all_items$ies_10, all_items$ies_15, all_items$ies_18,
  all_items$ies_19, all_items$ies_21
)
reliability(hyperarousal)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.69  0.85      0.87 0.96  0.97   0.99      0.87      0.81   0.48  0.54       6

iesr <- cbind(avoiding, intrusivity, hyperarousal)
reliability(iesr)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.76  0.93      0.94 0.86  0.88   0.98      0.96      0.87   0.39  0.39      22



# SCS ---------------------------------------------------------------------


# Self-Kindness
self_kindness <- tibble(
  all_items$scs_5, all_items$scs_12, all_items$scs_19, all_items$scs_23,
  all_items$scs_26
)
reliability(self_kindness)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items     0.6  0.84       0.9 0.94  0.96   0.98      0.82      0.77   0.52  0.51       5

# Self-Judgment
self_judgment <- tibble(
  scs_1 = abs(all_items$scs_1 - 6), scs_8 = abs(all_items$scs_8 - 6),
  scs_11 = abs(all_items$scs_11 - 6), scs_16 = abs(all_items$scs_16 - 6),
  scs_21 = abs(all_items$scs_21 - 6)
)
reliability(self_judgment)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.72  0.84      0.85 0.97  0.97      1      0.84      0.75    0.5  0.49       5

# Common Humanity
common_humanity <- tibble(
  all_items$scs_3, all_items$scs_7, all_items$scs_10, all_items$scs_15
)
reliability(common_humanity)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.56  0.72      0.78 0.92  0.94   0.98      0.77      0.63   0.39  0.35       4

# Isolation
isolation <- tibble(
  scs_4 = abs(all_items$scs_4 - 6), scs_13 = abs(all_items$scs_13 - 6),
  scs_18 = abs(all_items$scs_18 - 6), scs_25 = abs(all_items$scs_25 - 6)
)
reliability(isolation)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.78  0.86      0.89 0.99  0.99      1      0.89      0.83   0.61   0.6       4

# Mindfulness
mindfulness <- tibble(
  all_items$scs_9, all_items$scs_14, all_items$scs_17, all_items$scs_22
)
reliability(mindfulness)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.68  0.75      0.78 0.95  0.96   0.99      0.78       0.7   0.42  0.43       4

# Overidentification
over_identification <- tibble(
  scs_2 = abs(all_items$scs_2 - 6), scs_6 = abs(all_items$scs_6 - 6),
  scs_20 = abs(all_items$scs_20 - 6), scs_24 = abs(all_items$scs_24 - 6)
)
reliability(over_identification)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items     0.7  0.83      0.86 0.99  0.99      1      0.86      0.79   0.54  0.52       4

self_compassion <- cbind(
  self_judgment, all_items$isolation, over_identification,
  self_kindness, all_items$common_humanity, mindfulness
)
reliability(self_compassion)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.46  0.89      0.92 0.59  0.74    0.8      0.94      0.51    0.3  0.28      20




# NEO-FFI-60 --------------------------------------------------------------

# NEO-FFI-60 Neuroticism

# Negative affect:
negative_affect <- tibble(
  neoffi_1 = abs(all_items$neoffi_1 - 4),
  neoffi_11 = all_items$neoffi_11,
  neoffi_16 = abs(all_items$neoffi_16 - 4), 
  neoffi_31 = abs(all_items$neoffi_31 - 4), 
  neoffi_46 = abs(all_items$neoffi_46 - 4)
)

# Self reproach:
self_reproach <- tibble(
  all_items$neoffi_6,  
  all_items$neoffi_21, 
  all_items$neoffi_26, 
  all_items$neoffi_36,
  all_items$neoffi_41, 
  all_items$neoffi_51, 
  all_items$neoffi_56
)

neuroticism <- cbind(negative_affect, self_reproach)

reliability(neuroticism)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.69  0.88      0.89 0.88   0.9   0.98      0.91      0.79   0.38  0.38      12

# NEO-FFI-60 Extraversion

# The second factor of all_items is called *extraversion* and is composed by
# three subscales. The scores on such scales are the following:

extraversion <- tibble(
  all_items$neoffi_7, 
  neoffi_12 = abs(all_items$neoffi_12 - 4),
  all_items$neoffi_37,
  neoffi_42 = abs(all_items$neoffi_42 - 4),
  all_items$neoffi_2, 
  all_items$neoffi_17, 
  neoffi_27 = abs(all_items$neoffi_27 - 4),
  neoffi_57 = abs(all_items$neoffi_57 - 4), 
  all_items$neoffi_22,
  all_items$neoffi_32,
  all_items$neoffi_47,
  all_items$neoffi_52
)

reliability(extraversion)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.39  0.74      0.77 0.62  0.71   0.87      0.83      0.59   0.19  0.18      12


# NEO-FFI-60 Openness

openness <- tibble(
    all_items$neoffi_13,
    neoffi_23 = abs(all_items$neoffi_23 - 4),
    all_items$neoffi_43,
    neoffi_48 = abs(all_items$neoffi_48 - 4),
    all_items$neoffi_53,
    all_items$neoffi_58,
    neoffi_3 = abs(all_items$neoffi_3 - 4), 
    neoffi_8 = abs(all_items$neoffi_8 - 4),
    neoffi_18 = abs(all_items$neoffi_18 - 4), 
    neoffi_38 = abs(all_items$neoffi_38 - 4),
    all_items$neoffi_28,
    neoffi_33 = abs(all_items$neoffi_33 - 4)
)

reliability(openness)
#           omega_h alpha omega.tot Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.22  0.59       0.6 0.4  0.52   0.77      0.74      0.44   0.11  0.11      12


# NEO-FFI-60 Agreableness

agreableness <- tibble(

  neoffi_9 = abs(all_items$neoffi_9 - 4),
  neoffi_14 = abs(all_items$neoffi_14 - 4),
  neoffi_19 = all_items$neoffi_19,
  neoffi_24 = abs(all_items$neoffi_24 - 4),
  neoffi_29 = abs(all_items$neoffi_29 - 4),
  neoffi_44 = abs(all_items$neoffi_44 - 4),
  neoffi_54 = abs(all_items$neoffi_54 - 4),
  neoffi_59 = abs(all_items$neoffi_59 - 4),
  
  all_items$neoffi_4,
  all_items$neoffi_34,
  neoffi_39 = abs(all_items$neoffi_39 - 4),
  all_items$neoffi_49
)

reliability(agreableness)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.04  0.59      0.64 0.27  0.41   0.65      0.72      0.31   0.11  0.12      12


# NEO-FFI-60 Consciensciousness

consciensciousness <- tibble(
  # Orderliness:
  all_items$neoffi_5,
  all_items$neoffi_10,
  neoffi_15 = abs(all_items$neoffi_15 - 4),
  neoffi_30 = abs(all_items$neoffi_30 - 4),
  neoffi_55 = abs(all_items$neoffi_55 - 4),

  all_items$neoffi_25,
  all_items$neoffi_35,
  all_items$neoffi_60,
  
  # Dependability:
  all_items$neoffi_20,
  all_items$neoffi_40,
  neoffi_45 = abs(all_items$neoffi_45 - 4),
  all_items$neoffi_50
)

reliability(consciensciousness)
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.37  0.78      0.82 0.63  0.74   0.85      0.86      0.57   0.23  0.22      12

neoffi60 <- cbind(
  neuroticism,
  extraversion,
  openness,
  agreableness,
  consciensciousness
)

reliability(neoffi60)


# MSPSS -------------------------------------------------------------------

family <- cbind(
  all_items$mspss_3, all_items$mspss_4, all_items$mspss_8, all_items$mspss_11
)
reliability(family)

friends <- cbind(
  all_items$mspss_6, all_items$mspss_7, all_items$mspss_9, all_items$mspss_12
)

significant_other <- cbind(
  all_items$mspss_1, all_items$mspss_2, all_items$mspss_5, all_items$mspss_10
)

m <- '
  fa  =~ mspss_3 + mspss_4 + mspss_8 + mspss_11
  fr =~ mspss_6 + mspss_7 + mspss_9 + mspss_12
  so  =~ mspss_1 + mspss_2 + mspss_5 + mspss_10
'

fit <- lavaan::cfa(m, data = all_items, ordered = TRUE, std.lv = TRUE)
compRelSEM(fit, return.total = -1)
#    fa    fr    so 
# 0.944 0.961 0.946 
