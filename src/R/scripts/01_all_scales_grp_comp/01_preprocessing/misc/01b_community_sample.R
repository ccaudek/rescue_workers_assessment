# Script name: 01b_community_sample.R
# Project: Self-compassion: one construct or two?
# Script purpose: Import data; community sample.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Feb  4 10:55:28 2020
# Last Modified Date: Sat Sep 17 06:31:04 2022
#
# 👉 In the self-compassion project, the following scales have been 
# administered.
#
# - NEO FFI 60: 
#   neuroticism extraversion openness agreeableness conscientiousness
# 
# - COPE:
#   social_support avoiding_strategies positive_attitude 
#   problem_orientation transcendent_orientation 
# coping <- social_support + positive_attitude + problem_orientation + 
#           transcendent_orientation
# 
# - PTG:
#   interpersonal_relationships new_possibilities personal_strength 
#   life_appreciation spirituality_changes 
# ptg <- life_appreciation + new_possibilities + personal_strength + 
#        spirituality_changes + interpersonal_relationships
# 
# - IES-R (The Impact of Event Scale-Revised):
#   avoiding intrusivity iperarousal (components of PTSD)
# ies_r_score <- avoiding + intrusivity + iperarousal
# 
# - SCS:
#   self_kindness self_judgment common_humanity isolation mindfulness 
#   over_identification
# neg_self_compassion <- self_judgment + isolation + over_identification
# pos_self_compassion <- self_kindness + common_humanity + mindfulness
# 
# - MSPSS:
#   family friends significant_other 


# Set up ----

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
library("forcats")
library("readxl")
library("mice")

options(max.print=999999)

set.seed(12345)

# Read data ----

df <- read_excel(here("data", "raw", "non_soccorritori_20210404.xlsx"))

# df$where <- "toscana"

# Rename variables
df <- df %>% 
  rename(
    date = `Informazioni cronologiche`,
    permission = `Trattamento dei dati personali`,
    id = `Inserisci il codice identificativo come sopra indicato:`,
    gender = Sesso,
    age =  `Quanti anni hai?`,
    education = `Livello istruzione`,
    employment =  Occupazione
  ) 

# Fix age.
df$age[1] <- NA
df$age <- as.numeric(df$age)

# The data.frame df contains the following scales:
# NEO-FFI -   9:68
# COPE    -  69:128
# PTGI    - 129:151
# IES-R   - 152:175
# SCS     - 176:201
# MSPSS   - 202:213


# Imputation ----

# The variables' names cannot contain special characters.
temp <- df
# Create fake columns' names.
fake_names <- paste0("fake_name_", 1:ncol(df))
names(temp) <- fake_names
# Imputation with pmm method. 
imp <- mice(temp, meth = 'pmm', m = 5, maxit = 1, seed = 12345)
completed_data <- complete(imp, 1)
# Replace fake columns' names with original columns names.
names(completed_data) <- names(df)

df <- completed_data

# Correct age: minimum 18.
df$age <- ifelse(df$age < 18, 18, df$age)


# NEO-FIve Factor Inventory (NEO-FFI) ----

# NEO-FFI measures five core personality traits: 
# neuroticism, openness, agreeableness, conscientiousness.

# First, let's select the right columns:
neo_ffi_60 <- df[, 9:68]

# Then we change the column names to facilitate coding
item_names <- paste0("i", 1:60)
names(neo_ffi_60) <- item_names

# Some items are reversed. In such cases, the correct values are given 
# by: abs(x - max).

# Neuroticism

# The first factor of neo_ffi_60 is called *neuroticism* and is composed 
# by two subscales. The scores on such scales are the following:

# Negative affect:
negative_affect <- 
  abs(neo_ffi_60$i1 - 4) + neo_ffi_60$i11 + abs(neo_ffi_60$i16 - 4) + 
  abs(neo_ffi_60$i31 - 4) + abs(neo_ffi_60$i46 - 4)

# Self reproach:
self_reproach <-
  neo_ffi_60$i6 + neo_ffi_60$i21 + neo_ffi_60$i26 + neo_ffi_60$i36 + 
  neo_ffi_60$i41 + neo_ffi_60$i51 + neo_ffi_60$i56

# neuroticism scores
neuroticism <- negative_affect + self_reproach

# Just to take a look to the data, an histogram of neuroticism is
# hist(neuroticism)

# Extroversion
# The second factor of neo_ffi_60 is called *extraversion* and is composed by 
# three subscales. The scores on such scales are the following:
positive_affect <- 
  neo_ffi_60$i7 + abs(neo_ffi_60$i12 - 4) + neo_ffi_60$i37 + 
  abs(neo_ffi_60$i42 -4)

sociability <- 
  neo_ffi_60$i2 + neo_ffi_60$i17 + abs(neo_ffi_60$i27 - 4) + 
  abs(neo_ffi_60$i57 - 4)

activity <- 
  neo_ffi_60$i22 + neo_ffi_60$i32 + neo_ffi_60$i47 + neo_ffi_60$i52

extraversion <- 
  positive_affect + sociability + activity

# Openness
# The third factor of neo_ffi_60 is called *openness* and is composed 
# by three subscales. The scores on such scales are the following.

# Aesthetic interests:
aesthetic_interests <- 
  neo_ffi_60$i13 + abs(neo_ffi_60$i23 - 4) + neo_ffi_60$i43

# Intellectual interests:
intellectual_interests <- 
  abs(neo_ffi_60$i48 - 4) + neo_ffi_60$i53 + neo_ffi_60$i58

# Unconventionality:
unconventionality <- 
  abs(neo_ffi_60$i3 - 4) + abs(neo_ffi_60$i8 - 4) + 
  abs(neo_ffi_60$i18 - 4) + abs(neo_ffi_60$i38 - 4) + neo_ffi_60$i28 + 
  abs(neo_ffi_60$i33 - 4)

# Openness scores
openness <- aesthetic_interests + intellectual_interests + 
  unconventionality
# hist(openness)

# Agreeableness
# The fourth factor of neo_ffi_60 is called *agreeableness* and 
# is composed by two subscales. 

# Nonantagonistic orientation:
nonantagonistic_orientation <- 
  abs(neo_ffi_60$i9 - 4) + abs(neo_ffi_60$i14 - 4) + 
  neo_ffi_60$i19 + abs(neo_ffi_60$i24 - 4) + 
  abs(neo_ffi_60$i29 - 4) + abs(neo_ffi_60$i44 - 4) + 
  abs(neo_ffi_60$i54 - 4) + abs(neo_ffi_60$i59 - 4)

# Prosocial orientation:
prosocial_orientation <- 
  neo_ffi_60$i4 + neo_ffi_60$i34 + abs(neo_ffi_60$i39 - 4) + 
  neo_ffi_60$i49

# agreeableness scores
agreeableness <- nonantagonistic_orientation + prosocial_orientation
# hist(agreeableness)

# Conscientiousness
# The fifth factor of neo_ffi_60 is called *conscientiousness* and 
# is composed by three subscales. 

# Orderliness:
orderliness <- 
  neo_ffi_60$i5 + neo_ffi_60$i10 + abs(neo_ffi_60$i15 - 4) + 
  abs(neo_ffi_60$i30 - 4) + abs(neo_ffi_60$i55 - 4)

# Goal striving:
goal_striving <- neo_ffi_60$i25 + neo_ffi_60$i35 + neo_ffi_60$i60

# Dependability:
dependability <- 
  neo_ffi_60$i20 + neo_ffi_60$i40 + abs(neo_ffi_60$i45 - 4) + 
  neo_ffi_60$i50

# conscientiousness scores
conscientiousness <- orderliness + goal_striving + dependability
# hist(conscientiousness)


# Coping Orientation to Problems Experienced (COPE-NVI) ----

# COPE-NVI was devised to measure coping strategies on five dimensions: 
# - problem-oriented coping strategies, 
# - avoidance strategies, 
# - social-support strategies,
# - positive attitude,
# - transcendent orientation.
#
# Supporto sociale (Ricerca di comprensione + Ricerca di informazioni + 
# Sfogo emotivo), Strategie di evitamento (Negazione + Umorismo + 
# Uso di droga + Distacco comportamentale +  Distacco mentale), 
# Attitudine positiva (Accettazione + Contenimento + 
# Reinterpretazione positiva), Orientamento al problema (Soppressione + 
# Pianifi cazione + Attività), Religione (Sica, Novara, Dorz e Sanavio, 1997a)

# [69] "Cerco di utilizzare questa esperienza per crescere come persona"  Item 1 
# [128] "Prego più del solito"     Item 60

# First, we select the right columns:
cope_nvi <- df[, 69:128]

# Then we change the column names to facilitate coding
item_names <- paste0("ii", 1:60)
names(cope_nvi) <- item_names

# Some items are reversed. In that cases, we can compute the right 
# values by using the following formula: abs(x - max).

# Social support
# The first subscale of cope_nvi is called social support. 
# The scores on such scales are the following:
social_support <- 
  cope_nvi$ii4 + cope_nvi$ii14 + cope_nvi$ii30 + cope_nvi$ii45 + 
  cope_nvi$ii11 + cope_nvi$ii23 + cope_nvi$ii34 + cope_nvi$ii52 + 
  cope_nvi$ii3 + cope_nvi$ii17 + cope_nvi$ii28 + cope_nvi$ii46
# hist(social_support)

# Avoiding strategies
# The second subscale of cope_nvi is called avoiding strategies. 
# The scores on such scale are the following:
avoiding_strategies <- 
  cope_nvi$ii6 + cope_nvi$ii27 + cope_nvi$ii40 + cope_nvi$ii57 + 
  cope_nvi$ii9 + cope_nvi$ii24 + cope_nvi$ii37 + cope_nvi$ii51 + 
  cope_nvi$ii2 + cope_nvi$ii16 + cope_nvi$ii31 + cope_nvi$ii43 +  
  cope_nvi$ii12 +  cope_nvi$ii26 + cope_nvi$ii35 + cope_nvi$ii53
# hist(avoiding_strategies)

# Positive attitude
# The third subscale of cope_nvi is called positive attitude. 
# The scores on such scale are the following:
positive_attitude <- cope_nvi$ii10 + cope_nvi$ii22 + 
  cope_nvi$ii41 + cope_nvi$ii49 + cope_nvi$ii1 + cope_nvi$ii29 + 
  cope_nvi$ii38 + cope_nvi$ii59 + cope_nvi$ii13 + cope_nvi$ii21 + 
  cope_nvi$ii44 + cope_nvi$ii54
# hist(positive_attitude)

# Problem orientation
# The fourth subscale of cope_nvi is called problem orientation. 
# The scores on such scale are the following:
problem_orientation <- cope_nvi$ii5 + cope_nvi$ii25 + cope_nvi$ii47 + 
  cope_nvi$ii58 + cope_nvi$ii19 + cope_nvi$ii32 + cope_nvi$ii39 + 
  cope_nvi$ii56 + cope_nvi$ii15 + cope_nvi$ii33 + cope_nvi$ii42 + 
  cope_nvi$ii55
# hist(problem_orientation)

# Transcendent orientation
# The fifth subscale of cope_nvi is called transcendent orientation. 
# The scores on such scale are the following:
transcendent_orientation <- abs(cope_nvi$ii8 - 5) + 
  abs(cope_nvi$ii20 - 5) + abs(cope_nvi$ii36 - 5) + 
  abs(cope_nvi$ii50 - 5) + cope_nvi$ii7 + 
  cope_nvi$ii18 + cope_nvi$ii48 + cope_nvi$ii60
# hist(transcendent_orientation)

# avoinding strategies Reversed:
avoiding_strategies_R <- 
  abs(cope_nvi$ii6 - 5) + abs(cope_nvi$ii27 - 5) + 
  abs(cope_nvi$ii40 - 5) + abs(cope_nvi$ii57 - 5) + 
  abs(cope_nvi$ii9 - 5) + abs(cope_nvi$ii24 - 5) + 
  abs(cope_nvi$ii37 - 5) + abs(cope_nvi$ii51 - 5) + 
  abs(cope_nvi$ii2 - 5) + abs(cope_nvi$ii16 - 5) + 
  abs(cope_nvi$ii31 - 5) + abs(cope_nvi$ii43 - 5) +  
  abs(cope_nvi$ii12 - 5) + abs(cope_nvi$ii26 - 5) + 
  abs(cope_nvi$ii43 - 5) + abs(cope_nvi$ii35 - 5) + 
  abs(cope_nvi$ii53 - 5)

# trascendent orientation Reversed
transcendent_orientation_R <- cope_nvi$ii8 + 
  cope_nvi$ii20 + cope_nvi$ii36 + cope_nvi$ii50 + 
  abs(cope_nvi$ii7 - 5) + abs(cope_nvi$ii18 - 5) + 
  abs(cope_nvi$ii48 - 5) + abs(cope_nvi$ii60 - 5)

# total score
cope_nvi <- social_support + avoiding_strategies + 
  positive_attitude + problem_orientation + 
  transcendent_orientation


# Post Traumatic Growth Inventory (PTGI) ----

# PTGI is an instrument for assessing positive outcomes reported 
# by persons who have experienced traumatic events.

# [131] "Ho cambiato le priorità della mia vita" Item 1
# [151] "Accetto meglio il fatto di aver bisogno di altri" Item 21

# We rename column and variables of PTG event as follows
life_event_ptg <- df[, 129]
time_life_event_ptg <- df[, 130]

# Let start with the coding of ptgi. First, we select the 
# right columns:
ptgi <- df[, 131:151]

# Then we change the column names to facilitate coding
item_names <- paste0("iii", 1:21)
names(ptgi) <- item_names

# Relating to Others
interpersonal_relationships <-
  ptgi$iii15 + ptgi$iii20 + ptgi$iii9 + ptgi$iii21 + 
  ptgi$iii8 + ptgi$iii16 + ptgi$iii6
# hist(interpersonal_relationships)

# New Possibilities
new_possibilities <- ptgi$iii11 + ptgi$iii7 + ptgi$iii3 + 
  ptgi$iii17 + ptgi$iii14
# hist(new_possibilities)

# Personal Strength
personal_strength <- ptgi$iii10 + ptgi$iii19 + ptgi$iii4 + 
  ptgi$iii12
# hist(personal_strength)

# Appreciation
life_appreciation <- ptgi$iii13 + ptgi$iii2 + ptgi$iii1
# hist(life_appreciation)

# Spiritual Enhancement
spirituality_changes <- ptgi$iii5 + ptgi$iii18
# hist(spirituality_changes)

ptg <- life_appreciation + new_possibilities + 
  personal_strength + spirituality_changes + 
  interpersonal_relationships


# Impact of Event Scale - Revised (IES-R) ----

# The IES-R is a 22-item self-report measure (for DSM-IV) that assesses 
# subjective distress caused by traumatic events.
# The IES-R yields a total score (ranging from 0 to 88) and subscale scores 
# can also be calculated for the Intrusion, Avoidance, and Hyperarousal subscales.
# Il massimo punteggio medio di ognuna delle 3 subscale è 4, quindi il 
# punteggio medio totale massimo della scala IES-R è 12. 
# Bassi punteggi sono migliori. 
# Un punteggio totale alla IES-R di 33 o superiore su un punteggio massimo di 88 
# significa la probabile presenza di un PTSD.

# [152] df[, 152] "Descrivi brevemente l'evento traumatico:"   
# [153] df[, 153] "Quanto tempo è passato dall'evento?...153"   

# First, we select the right columns:

traumatic_event_ies <- df[, 152]
time_traumatic_event_ies <- df[, 153]

ies_r <- df[, 154:175]

# Then we change the column names to facilitate coding
item_names <- paste0("iiii", 1:22)
names(ies_r) <- item_names

# Avoidance
avoiding <- 
  ies_r$iiii5 + ies_r$iiii7 + ies_r$iiii8 + ies_r$iiii11 + 
  ies_r$iiii12 + ies_r$iiii13 + ies_r$iiii17 + ies_r$iiii22
# hist(avoiding)

# Intrusion
intrusivity <- 
  ies_r$iiii1 + ies_r$iiii2 + ies_r$iiii3 + ies_r$iiii6 + 
  ies_r$iiii9 + ies_r$iiii14 + ies_r$iiii16 + ies_r$iiii20
# hist(intrusivity)

# Hyperarousal
iperarousal <-  
  ies_r$iiii4 + ies_r$iiii10 + ies_r$iiii15 + ies_r$iiii18 + 
  ies_r$iiii19 + ies_r$iiii21
# hist(iperarousal)

# Total score
ies_r_score <- avoiding + intrusivity + iperarousal
# hist(ies_r_score)


# Self-Compassion Scale (SCS) ----

# The Self-Compassion Scale (SCS) is used to assess how people 
# treated themselves in difficult times (Neff 2003a). 
# It includes 26 items across six subscales: 
# - Self-Kindness, 
# - Self-Judgment, 
# - Common Humanity, 
# - Isolation, 
# - Mindfulness, 
# - Overidentification
# I punteggi delle sottoscale del scs sono ottenuti calcolando la 
# media delle risposte nei rispettivi item.
# Quindi calcolare la media dei punteggi medi delle sei sottoscale. 
# COME SI FA?
# In this approach any mean scale score ranging from 1 to 2.9 could 
# be considered low support; a score of 3 to 5 could be considered 
# moderate support; a score from 5.1  to 7 could be considered high 
# support. This approach would seem to have more validity, but if you 
# have very few respondents in any of the groups, it could be problematic.

# [176] "Sono critico/a e severo/a nei confronti dei miei difetti e delle mie inadeguatezze" Item 1
# [201] "Cerco di essere comprensivo/a e paziente verso quegli aspetti della mia personalità che non mi piacciono"                                                         
# Item 21

# First, we select the right columns:
scs <- df[, 176:201]

# Then we change the column names to facilitate coding
item_names <- paste0("iiiii", 1:26)
names(scs) <- item_names

# Self-Kindness
self_kindness <- 
  scs$iiiii5 + scs$iiiii12 + scs$iiiii19 + scs$iiiii23 + 
  scs$iiiii26
# hist(self_kindness)

# Self-Judgment
self_judgment <- 
  abs(scs$iiiii1 - 6) + abs(scs$iiiii8 - 6) + 
  abs(scs$iiiii11 - 6) + abs(scs$iiiii16 - 6) + 
  abs(scs$iiiii21 - 6)
# hist(self_judgment)

# Common Humanity
common_humanity <- 
  scs$iiiii3 + scs$iiiii7 + scs$iiiii10 + scs$iiiii15
# hist(common_humanity)

# Isolation
isolation <- 
  abs(scs$iiiii4 - 6) + abs(scs$iiiii13 - 6) + 
  abs(scs$iiiii18 - 6) + abs(scs$iiiii25 - 6)
# hist(isolation)

# Mindfulness
mindfulness <- 
  scs$iiiii9 + scs$iiiii14 + scs$iiiii17 + scs$iiiii22
# hist(mindfulness)

# Overidentification
over_identification <- 
  abs(scs$iiiii2 - 6) + abs(scs$iiiii6 - 6) + 
  abs(scs$iiiii20 - 6) + abs(scs$iiiii24 - 6)
# hist(over_identification)

neg_self_compassion <- self_judgment + isolation + 
  over_identification

pos_self_compassion <- self_kindness + common_humanity + 
  mindfulness


# Multidimensional Scale of Perceived Social Support (MSPSS) ----

# The MSPSS is a 12-item scale designed to measure perceived social 
# support from three sources:
# - Family, 
# - Friends, 
# - a Significant other

# [202] "C'è una particolare persona che mi è vicina quando ho bisogno" Item 1
# [213] "Posso parlare dei miei problemi con i miei amici/amiche"   Item 12                                                                                                       

# First, we select the right columns:
mspss <- df[, 202:213]

# Then we change the column names to facilitate coding
item_names <- paste0("iiiiii", 1:12)
names(mspss) <- item_names

# Family
family <- 
  mspss$iiiiii3 + mspss$iiiiii4 + mspss$iiiiii8 + mspss$iiiiii11
# hist(family)

# Friends
friends <- 
  mspss$iiiiii6 + mspss$iiiiii7 + mspss$iiiiii9 + mspss$iiiiii12
# hist(friends)

# A significant other
significant_other <-
  mspss$iiiiii1 + mspss$iiiiii2 + mspss$iiiiii5 + mspss$iiiiii10
# hist(significant_other)


# Data wrangling ----

df$gender <- fct_recode(
  df$gender, 
  "female" = "Femmina", 
  "male" = "Maschio"
)

df$is_rescue_worker <- "no"

df$employment <- fct_recode(
  df$employment, 
  "Dipendente" = "Operaio",
  "Dipendente" = "Impiegato",
  "Libero professionista" = "Imprenditore",
  "Dipendente" = "Dipendente CRI",
  "Dipendente" = "Infermiere",
  "Dipendente" = "Militare",
  "Dipendente" = "Vigile del Fuoco",
  "Pensionato" = "pensionato",
  "Dipendente" = "Funzionario Pubblico",
  "Dipendente" = "Forze dell'Ordine",
  "Dipendente" = "Ricercatore",
  "Libero professionista" = "Commerciante", 
  "Dipendente" = "Educatore",
  "Dipendente" = "Soccorritore",
  "Dipendente" = "Polizia locale",
  "Dipendente" = "Polizia Locale",
  "Studente" = "Dottoranda",
  "Dipendente" = "Medico",
  "Pensionato" = "Pensione",
  "Dipendente" = "Insegnante"
)

# df$is_job_qualification_invariant <- fct_recode(
#   df$is_job_qualification_invariant, 
#   "Yes" = "Sì",
#   "Yes" = "Si"
# )
# table(df$is_job_qualification_invariant)


# final data frame:
thedat <- data.frame(
  where = "toscana",
  id = factor(df$id),
  gender = factor(df$gender),
  age = df$age,
  education = factor(df$education),
  employment = factor(df$employment),
  is_rescue_worker = factor(df$is_rescue_worker),
  # NEO-FFI
  neuroticism,
  negative_affect,
  i1_na = abs(neo_ffi_60$i1 - 4),
  i2_na = neo_ffi_60$i11,
  i3_na = abs(neo_ffi_60$i16 - 4),
  i4_na = abs(neo_ffi_60$i31 - 4),
  i5_na = abs(neo_ffi_60$i46 -4),
  i1_sr = neo_ffi_60$i6,
  i2_sr = neo_ffi_60$i21,
  i3_sr = neo_ffi_60$i26,
  i4_sr = neo_ffi_60$i36,
  i5_sr = neo_ffi_60$i41,
  i6_sr = neo_ffi_60$i51,
  i7_sr = neo_ffi_60$i56,
  self_reproach,
  extraversion,
  openness,
  agreeableness,
  nonantagonistic_orientation, 
  prosocial_orientation,
  conscientiousness,
  # COPE
  cope_nvi,
  social_support,
  avoiding_strategies,
  positive_attitude,
  problem_orientation,
  transcendent_orientation,
  avoiding_strategies_R, # reversed
  transcendent_orientation_R, # reversed
  # PTG
  ptg, 
  life_appreciation,
  new_possibilities,
  personal_strength,
  spirituality_changes,
  interpersonal_relationships,
  # IES
  ies = ies_r_score,
  avoiding,
  intrusivity,
  iperarousal,
  # self-compassion scale:
  self_kindness,
  self_judgment, # reversed
  common_humanity, 
  isolation, # reversed
  mindfulness,
  over_identification, # reversed
  neg_self_compassion,
  pos_self_compassion,
  # MSPSS
  family, 
  friends, 
  significant_other
)


# Write data files to the "processed" directory:
saveRDS(thedat, file = here("data", "processed", "community_sample.Rds"))

# write.table(
#   thedat,
#   file = here("data", "processed", "community_sample.csv"),
#   sep = ";",
#   row.names = FALSE
# )

# eof ---




# e  n  d  ---




