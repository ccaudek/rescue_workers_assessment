

# NEO-FFI -----------------------------------------------------------------

# Neuroticism

# The first factor of all_items is called *neuroticism* and is composed 
# by two subscales. The scores on such scales are the following:

# Negative affect:
negative_affect <- abs(all_items$neoffi_1 - 4) + all_items$neoffi_11 + abs(all_items$neoffi_16 - 4) + 
  abs(all_items$neoffi_31 - 4) + abs(all_items$neoffi_46 -4)

# Self reproach:
self_reproach <- all_items$neoffi_6 + all_items$neoffi_21 + all_items$neoffi_26 + all_items$neoffi_36 + 
  all_items$neoffi_41 + all_items$neoffi_51 + all_items$neoffi_56

# neuroticism scores
all_items$neuroticism <- negative_affect + self_reproach

# Extroversion
# The second factor of all_items is called *extraversion* and is composed by 
# three subscales. The scores on such scales are the following:
positive_affect <- all_items$neoffi_7 + abs(all_items$neoffi_12 - 4) + all_items$neoffi_37 + 
  abs(all_items$neoffi_42 -4)

sociability <- all_items$neoffi_2 + all_items$neoffi_17 + abs(all_items$neoffi_27 - 4) + 
  abs(all_items$neoffi_57 - 4)

activity <- all_items$neoffi_22 + all_items$neoffi_32 + all_items$neoffi_47 + all_items$neoffi_52

all_items$extraversion <- positive_affect + sociability + activity

# Openness
# The third factor of all_items is called *openness* and is composed 
# by three subscales. The scores on such scales are the following.

# Aesthetic interests:
aesthetic_interests <- 
  all_items$neoffi_13 + abs(all_items$neoffi_23 - 4) + all_items$neoffi_43

# Intellectual interests:
intellectual_interests <- 
  abs(all_items$neoffi_48 - 4) + all_items$neoffi_53 + all_items$neoffi_58

# Unconventionality:
unconventionality <- 
  abs(all_items$neoffi_3 - 4) + abs(all_items$neoffi_8 - 4) + 
  abs(all_items$neoffi_18 - 4) + abs(all_items$neoffi_38 - 4) + all_items$neoffi_28 + 
  abs(all_items$neoffi_33 - 4)

# Openness scores
all_items$openness <- aesthetic_interests + intellectual_interests + 
  unconventionality

# Agreeableness
# The fourth factor of all_items is called *agreeableness* and 
# is composed by two subscales. 

# Nonantagonistic orientation:
nonantagonistic_orientation <- 
  abs(all_items$neoffi_9 - 4) + abs(all_items$neoffi_14 - 4) + 
  all_items$neoffi_19 + abs(all_items$neoffi_24 - 4) + 
  abs(all_items$neoffi_29 - 4) + abs(all_items$neoffi_44 - 4) + 
  abs(all_items$neoffi_54 - 4) + abs(all_items$neoffi_59 - 4)

# Prosocial orientation:
prosocial_orientation <- 
  all_items$neoffi_4 + all_items$neoffi_34 + abs(all_items$neoffi_39 - 4) + 
  all_items$neoffi_49

# agreeableness scores
all_items$agreeableness <- nonantagonistic_orientation + prosocial_orientation
# hist(agreeableness)

# Conscientiousness
# The fifth factor of all_items is called *conscientiousness* and 
# is composed by three subscales. 

# Orderliness:
orderliness <- all_items$neoffi_5 + all_items$neoffi_10 + abs(all_items$neoffi_15 - 4) + 
  abs(all_items$neoffi_30 - 4) + abs(all_items$neoffi_55 - 4)

# Goal striving:
goal_striving <- all_items$neoffi_25 + all_items$neoffi_35 + all_items$neoffi_60

# Dependability:
dependability <- all_items$neoffi_20 + all_items$neoffi_40 + abs(all_items$neoffi_45 - 4) + 
  all_items$neoffi_50

# conscientiousness scores
all_items$conscientiousness <- orderliness + goal_striving + dependability



# COPE-NVI ----------------------------------------------------------------

# Social support
all_items$social_support <- 
  all_items$cope_4 + all_items$cope_14 + all_items$cope_30 + all_items$cope_45 + 
  all_items$cope_11 + all_items$cope_23 + all_items$cope_34 + all_items$cope_52 + 
  all_items$cope_3 + all_items$cope_17 + all_items$cope_28 + all_items$cope_46


# Avoiding strategies
all_items$avoiding_strategies <- 
  all_items$cope_6 + all_items$cope_27 + all_items$cope_40 + all_items$cope_57 + 
  all_items$cope_9 + all_items$cope_24 + all_items$cope_37 + all_items$cope_51 + 
  all_items$cope_2 + all_items$cope_16 + all_items$cope_31 + all_items$cope_43 +  
  all_items$cope_12 +  all_items$cope_26 + all_items$cope_35 + all_items$cope_53

# Positive attitude
all_items$positive_attitude <- all_items$cope_10 + all_items$cope_22 + 
  all_items$cope_41 + all_items$cope_49 + all_items$cope_1 + all_items$cope_29 + 
  all_items$cope_38 + all_items$cope_59 + all_items$cope_13 + all_items$cope_21 + 
  all_items$cope_44 + all_items$cope_54


# Problem orientation
all_items$problem_orientation <- all_items$cope_5 + all_items$cope_25 + all_items$cope_47 + 
  all_items$cope_58 + all_items$cope_19 + all_items$cope_32 + all_items$cope_39 + 
  all_items$cope_56 + all_items$cope_15 + all_items$cope_33 + all_items$cope_42 + 
  all_items$cope_55


# Transcendent orientation
all_items$transcendent_orientation <- abs(all_items$cope_8 - 5) + 
  abs(all_items$cope_20 - 5) + abs(all_items$cope_36 - 5) + 
  abs(all_items$cope_50 - 5) + all_items$cope_7 + 
  all_items$cope_18 + all_items$cope_48 + all_items$cope_60


# total score
all_items$cope_total_score <- all_items$social_support + 
  all_items$avoiding_strategies + all_items$positive_attitude + 
  all_items$problem_orientation + all_items$transcendent_orientation



# PTGI --------------------------------------------------------------------


# Relating to Others
all_items$relating_to_others <-
  all_items$ptgi_15 + all_items$ptgi_20 + all_items$ptgi_9 + all_items$ptgi_21 + 
  all_items$ptgi_8 + all_items$ptgi_16 + all_items$ptgi_6

# New Possibilities
all_items$new_possibilities <- all_items$ptgi_11 + all_items$ptgi_7 + all_items$ptgi_3 + 
  all_items$ptgi_17 + all_items$ptgi_14

# Personal Strength
all_items$personal_strength <- all_items$ptgi_10 + all_items$ptgi_19 + all_items$ptgi_4 + 
  all_items$ptgi_12

# Appreciation of Life
all_items$appreciation_of_life <- all_items$ptgi_13 + all_items$ptgi_2 + all_items$ptgi_1

# Spiritual Enhancement
all_items$spirituality <- all_items$ptgi_5 + all_items$ptgi_18

all_items$ptgi_total_score <- all_items$appreciation_of_life + 
  all_items$new_possibilities + all_items$personal_strength + 
  all_items$spirituality + all_items$relating_to_others


# IES-R -------------------------------------------------------------------


# Avoidance
all_items$avoiding <- 
  all_items$ies_5 + all_items$ies_7 + all_items$ies_8 + all_items$ies_11 + 
  all_items$ies_12 + all_items$ies_13 + all_items$ies_17 + all_items$ies_22


# Intrusion
all_items$intrusivity <- 
  all_items$ies_1 + all_items$ies_2 + all_items$ies_3 + all_items$ies_6 + 
  all_items$ies_9 + all_items$ies_14 + all_items$ies_16 + all_items$ies_20


# Hyperarousal
all_items$hyperarousal <-  
  all_items$ies_4 + all_items$ies_10 + all_items$ies_15 + all_items$ies_18 + 
  all_items$ies_19 + all_items$ies_21


# Total score
all_items$ies_total_score <- all_items$avoiding + all_items$intrusivity + 
  all_items$hyperarousal


# SCS ---------------------------------------------------------------------

# Self-Kindness
all_items$self_kindness <- 
  all_items$scs_5 + all_items$scs_12 + all_items$scs_19 + all_items$scs_23 + 
  all_items$scs_26

# Self-Judgment
all_items$self_judgment <- 
  abs(all_items$scs_1 - 6) + abs(all_items$scs_8 - 6) + 
  abs(all_items$scs_11 - 6) + abs(all_items$scs_16 - 6) + 
  abs(all_items$scs_21 - 6)

# Common Humanity
all_items$common_humanity <- 
  all_items$scs_3 + all_items$scs_7 + all_items$scs_10 + all_items$scs_15

# Isolation
all_items$isolation <- 
  abs(all_items$scs_4 - 6) + abs(all_items$scs_13 - 6) + 
  abs(all_items$scs_18 - 6) + abs(all_items$scs_25 - 6)

# Mindfulness
all_items$mindfulness <- 
  all_items$scs_9 + all_items$scs_14 + all_items$scs_17 + all_items$scs_22

# Overidentification
all_items$over_identification <- 
  abs(all_items$scs_2 - 6) + abs(all_items$scs_6 - 6) + 
  abs(all_items$scs_20 - 6) + abs(all_items$scs_24 - 6)

all_items$neg_self_compassion <- all_items$self_judgment + all_items$isolation + 
  all_items$over_identification
all_items$pos_self_compassion <- all_items$self_kindness + all_items$common_humanity + 
  all_items$mindfulness



# MSPSS -------------------------------------------------------------------


# Family
all_items$family <- 
  all_items$mspss_3 + all_items$mspss_4 + all_items$mspss_8 + all_items$mspss_11

# Friends
all_items$friends <- 
  all_items$mspss_6 + all_items$mspss_7 + all_items$mspss_9 + all_items$mspss_12

# A significant other
all_items$significant_other <-
  all_items$mspss_1 + all_items$mspss_2 + all_items$mspss_5 + all_items$mspss_10

all_items$mpss_tot <- all_items$family + all_items$friends + all_items$significant_other 


# Write RDS file ----------------------------------------------------------

# The all-items data.frame has been cleaned by considering the careless  
# responding indices. It contains the single items of each scale, and the values
# of the sub-scales. It also provides information about gender, age, 
# is_rescue_worker, group (rescue_worker, community_sample, student_sample).
saveRDS(
  all_items, 
  file = here("data", "processed", "all_items", 
              "three_samples_all_items_and_subscales.Rds")
)
