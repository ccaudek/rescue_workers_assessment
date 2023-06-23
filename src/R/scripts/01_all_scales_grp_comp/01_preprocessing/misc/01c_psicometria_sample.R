# Script name: 01c_psiometria_sample.R
# Project: Self compassion: one construct or two?
# Script purpose: Descriptive analyses of the student data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Feb  4 10:55:28 2020
# Last Modified Date: Sat Sep 17 10:55:12 2022
# 
# Notes: In the self-compassion project, the following scales have been 
# administered.
#
# - NEO FFI 60: 
#   neuroticism 
# 
# - COPE:
#   positive_attitude, problem_orientation 
#   coping <- social_support + positive_attitude + problem_orientation + 
#           transcendent_orientation
# 
# - PTG:
#   interpersonal_relationships new_possibilities personal_strength 
#   life_appreciation spirituality_changes 
#   ptg <- life_appreciation + new_possibilities + personal_strength + 
#        spirituality_changes + interpersonal_relationships
# 
# - IES-R (The Impact of Event Scale-Revised):
#   avoiding intrusivity iperarousal (components of PTSD)
#   ies_r_score <- avoiding + intrusivity + iperarousal
# 
# - SCS:
#   self_kindness self_judgment common_humanity isolation mindfulness 
#   over_identification
#   neg_self_compassion <- self_judgment + isolation + over_identification
#   pos_self_compassion <- self_kindness + common_humanity + mindfulness
# 
# - MSPSS:
#   family friends significant_other 


# Set up ----

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
library("forcats")
library("readxl")
library("mice")
library("careless")
library("EnvStats")

source(here("src", "R", "functions", "self_compassion_fnc.R"))

options(max.print=999999)

set.seed(12345)

# Read data ----

df <- read_excel(here("data", "raw", "psicometria.xlsx"))

# Rename variables
df <- df %>% 
  rename(
    date = `Informazioni cronologiche`,
    permission = `Trattamento dei dati personali`,
    id = `Inserisci il tuo codice anonimo (esempio: ma_ro_1997_05_04_174_m)`,
    gender = Sesso,
    age =  `Quanti anni hai?`,
    education = `Livello istruzione`,
    employment =  Occupazione
  ) 

# Remove subjects with only NAs.
df <- df[!is.na(df$gender), ]

# Imputation ----

temp <- df
fake_names <- paste0("fake_name_", 1:ncol(df))
names(temp) <- fake_names
set.seed(12345)
imp <- mice(temp, meth = 'pmm', m = 1, maxit = 5)
completed_data <- complete(imp, 1)
# Replace fake columns' names with original columns names.
names(completed_data) <- names(df)
df <- completed_data

# For the 'psicometria' sample, the imputed data.frame df contains the following 
# scales:
# NEO-FFI -  36:47 (only Neuroticism)
# COPE    -  71:94 (only Positive attitude and Problem orientation)
# PTGI    -  96:116
# IES-R   -  49:70
# SCS     -  224:249
# MSPSS   -  117:128


# NEO-Five Factor Inventory (NEO-FFI) ----

# Only the items for the Negative Affect and Self Reproach subscales of the
# Neuroticism trait have been collected. In the df dataframe, these are the 
# numbers of the columns (square brackets) and the number of corresponding item 
# in the original NEO-FFI scale (in parenthesis):
#
# [36] "Non sono una persona che si preoccupa"  (1, NA)                                                                                                                     
# [37] "Mi sento spesso inferiore agli altri"  (6, SR)                                                                                                                    
# [38] "Quando sono molto stressato/a mi sembra di andare in frantumi"   (11, NA)                                                                                                    
# [39] "Raramente mi capita di sentirmi solo/a e triste"  (16, NA)                                                                                                             
# [40] "Mi capita spesso di sentirmi teso/a e nervoso/a" (21, SR)                                                                                                            
# [41] "Qualche volta sento di non valere nulla" (26, SR)                                                                                                             
# [42] "Non mi capita spesso di sentirmi ansioso/a o impaurito/a"  (31, NA)                                                                                               
# [43] "Spesso mi arrabbio per come gli altri mi trattano"  (36, SR)                                                                                                                  
# [44] "Quando le cose vanno male, mi capita troppo spesso di scoraggiarmi e di avere voglia di lasciar perdere tutto"  (41, SR)                                                      
# [45] "Raramente mi sento triste o depresso/a"  (46, NA)                                                                                                                             
# [46] "Spesso mi sento impotente e ho bisogno di qualcuno che risolva i miei problemi"  (51, SR)                                                                                     
# [47] "A volte provo talmente tanta vergogna che vorrei nascondermi"    (56, SR)    


# Negative Affect subscale.
neg_aff_columns <- c(36, 38, 39, 42, 45)
neg_aff_df <- df[, neg_aff_columns]
neg_aff_item_names <- c("i1", "i11", "i16", "i31", "i46")
names(neg_aff_df) <- neg_aff_item_names

negative_affect <- 
  abs(neg_aff_df$i1 - 4) + neg_aff_df$i11 + abs(neg_aff_df$i16 - 4) + 
  abs(neg_aff_df$i31 - 4) + abs(neg_aff_df$i46 - 4)

# Self Reproach subscale.
self_repr_columns <- c(37, 40, 41, 43, 44, 46, 47)
self_repr_df <- df[, self_repr_columns]
self_repr_item_names <- c("i6", "i21", "i26", "i36", "i41", "i51", "i56")
names(self_repr_df) <- self_repr_item_names

self_reproach <-
  self_repr_df$i6 + self_repr_df$i21 + self_repr_df$i26 + self_repr_df$i36 + 
  self_repr_df$i41 + self_repr_df$i51 + self_repr_df$i56

# neuroticism scores
neuroticism <- negative_affect + self_reproach

# Possible outliers for careless responding.
neuroticism_items <- bind_cols(neg_aff_df, self_repr_df)
outliers_neuroticism <- outliers_carless_responding(neuroticism_items)


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

# [71] "Cerco di utilizzare questa esperienza per crescere come persona" (1)                                                                                                     
# [72] "Mi impegno al massimo per agire sulla situazione"  (5)                                                                                                                   
# [73] "Mi trattengo dall’agire troppo in fretta" (10)                                                                                                                           
# [74] "Cerco di abituarmi all’idea che ciò è successo"  (13)                                                                                                                      
# [75] "Non mi faccio distrarre da altri pensieri o attività"  (15)                                                                                                              
# [76] "Preparo un piano d’azione"  (19)                                                                                                                                           
# [77] "Accetto che ciò sia accaduto e che non possa essere cambiato"  (21)                                                                                                       
# [78] "Mi trattengo dal fare qualsiasi cosa fino a che la situazione lo permetta" (22)                                                                                          
# [79] "Mi sforzo più del solito per tentare di liberarmi dal problema"  (25)                                                                                                      
# [80] "Cerco di vedere le cose in una luce diversa, per farle sembrare più positive"  (29)                                                                                       
# [81] "Cerco di escogitare una strategia sul da farsi"  (32)                                                                                                                     
# [82] "Mi concentro nel trattare questo problema, e se necessario metto da parte le altre cose"  (33)                                                                            
# [83] "Cerco qualcosa di positivo in ciò che sta accadendo"  (38)                                                                                                                
# [84] "Penso a come potrei gestire al meglio il problema"  (39)                                                                                                                  
# [85] "Mi accerto di non peggiorare le cose agendo troppo presto"  (41)                                                                                                          
# [86] "Cerco strenuamente di impedire che le altre cose interferiscano coi miei sforzi di fronteggiare il problema"  (42)                                                        
# [87] "Accetto la realtà dei fatti"  (44)                                                                                                                                        
# [88] "Agisco senza indugio per sbarazzarmi del problema"  (47)                                                                                                                  
# [89] "Mi sforzo di aspettare il momento giusto per fare qualcosa" (49)                                                                                                          
# [90] "Imparo a convivere con il problema"  (54)                                                                                                                                 
# [91] "Metto da parte le altre attività per concentrarmi sulla situazione"  (55)                                                                                                 
# [92] "Rifletto intensamente su quali mosse fare"  (56)                                                                                                                          
# [93] "Faccio quello che deve essere fatto, un passo alla volta"  (58)                                                                                                           
# [94] "Tento di imparare qualcosa dall’esperienza"  (59)      

# First, we select the right columns:
cope_nvi <- df[, 71:94]

# Positive attitude subscale.
pos_att_columns <- c(71, 73,  74,  77, 78,  80,  83,  85, 87,  89,  90, 94)
pos_att_df <- df[, pos_att_columns]
pos_att_item_names <- c("ii1", "ii10", "ii13", "ii21", "ii22", "ii29", "ii38", 
                        "ii41", "ii44", "ii49", "ii54", "ii59") 
names(pos_att_df) <- pos_att_item_names

positive_attitude <- pos_att_df$ii1 + pos_att_df$ii10 +  pos_att_df$ii13 + 
  pos_att_df$ii21 + pos_att_df$ii22 + pos_att_df$ii29 + pos_att_df$ii38 + 
  pos_att_df$ii41 + pos_att_df$ii44 + pos_att_df$ii49 + pos_att_df$ii54 + 
  pos_att_df$ii59 


# Problem orientation subscale.
prob_or_columns <- c(72, 75,  76,  79, 81,  82,  84,  86, 88,  91,  92, 93)
prob_or_df <- df[, prob_or_columns]
prob_or_item_names <- c("ii5", "ii15", "ii19", "ii25", "ii32", "ii33", "ii39", 
                        "ii42", "ii47", "ii55", "ii56", "ii58") 
names(prob_or_df) <- prob_or_item_names

problem_orientation <- prob_or_df$ii5 + prob_or_df$ii15 + prob_or_df$ii19 + 
  prob_or_df$ii25 + prob_or_df$ii32 + prob_or_df$ii33 + prob_or_df$ii39 + 
  prob_or_df$ii42 + prob_or_df$ii47 + prob_or_df$ii55 + prob_or_df$ii56 + 
  prob_or_df$ii58 

# Possible outliers for careless responding.
cope_items <- bind_cols(pos_att_df, prob_or_df)
outliers_cope <- outliers_carless_responding(cope_items)


# Post Traumatic Growth Inventory (PTGI) ----

# PTGI is an instrument for assessing positive outcomes reported 
# by persons who have experienced traumatic events.

# [96] "Ho cambiato le priorità della mia vita" Item 1
# [116] "Accetto meglio il fatto di aver bisogno di altri" Item 21

# We rename column and variables of PTG event as follows
# life_event_ptg <- df[, 129]
# time_life_event_ptg <- df[, 130]

# Let start with the coding of ptgi. First, we select the 
# right columns:
ptgi <- df[, 96:116]

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

# Possible outliers for careless responding.
ptgi_items <- ptgi
outliers_ptgi <- outliers_carless_responding(ptgi_items)


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

# [49] "Ogni cosa che me lo ricordava mi faceva vivere emozioni relative ad esso"
# [70] "Ho cercato di non parlarne"

# traumatic_event_ies <- df[, 152]
# time_traumatic_event_ies <- df[, 153]

# First, we select the right columns:
ies_r <- df[, 49:70]

# Then we change the column names to facilitate coding
item_names <- paste0("iiii", 1:22)
names(ies_r) <- item_names

# Avoidance
avoiding <- 
  ies_r$iiii5 + ies_r$iiii7 + ies_r$iiii8 + ies_r$iiii11 + 
  ies_r$iiii12 + ies_r$iiii13 + ies_r$iiii17 + ies_r$iiii22
mean(avoiding, na.rm = TRUE)
# hist(avoiding)

# Intrusion
intrusivity <- 
  ies_r$iiii1 + ies_r$iiii2 + ies_r$iiii3 + ies_r$iiii6 + 
  ies_r$iiii9 + ies_r$iiii14 + ies_r$iiii16 + ies_r$iiii20
mean(intrusivity, na.rm = TRUE)
# hist(intrusivity)

# Hyperarousal
iperarousal <-  
  ies_r$iiii4 + ies_r$iiii10 + ies_r$iiii15 + ies_r$iiii18 + 
  ies_r$iiii19 + ies_r$iiii21
mean(iperarousal, na.rm = TRUE)
# hist(iperarousal)

# Total score
ies_r_score <- avoiding + intrusivity + iperarousal
# hist(ies_r_score)
mean(ies_r_score, na.rm = TRUE)

# Possible outliers for careless responding.
ies_r_items <- ies_r
outliers_ies_r <- outliers_carless_responding(ies_r_items)


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
scs <- df[, 224:249]

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

# Possible outliers for careless responding.
scs_items <- scs
outliers_scs <- outliers_carless_responding(scs_items)


# Multidimensional Scale of Perceived Social Support (MSPSS) ----

# The MSPSS is a 12-item scale designed to measure perceived social 
# support from three sources:
# - Family, 
# - Friends, 
# - a Significant other

# [202] "C'è una particolare persona che mi è vicina quando ho bisogno" Item 1
# [213] "Posso parlare dei miei problemi con i miei amici/amiche"   Item 12                                                                                                       

# First, we select the right columns:
mspss <- df[, 117:128]

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

# Possible outliers for careless responding.
mspss_items <- mspss
outliers_mspss <- outliers_carless_responding(mspss_items)


# Outliers detection ----

outliers <- c(
  outliers_neo_ffi = outliers_neuroticism,
  outliers_cope,
  outliers_ptgi,
  outliers_ies_r,
  outliers_scs,
  outliers_mspss
)
tbl <- base::table(outliers)
indices_outliers <- tbl[tbl> 3] %>% 
  names() %>% 
  as.numeric()
length(indices_outliers)

no_careless_resp_df <- df[-indices_outliers, ]

library("tidyLPA")

outliers %>%
  scale() %>%
  estimate_profiles(3) %>% 
  plot_profiles()



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
  i1_na = abs(neg_aff_df$i1 - 4),
  i2_na = neg_aff_df$i11,
  i3_na = abs(neg_aff_df$i16 - 4),
  i4_na = abs(neg_aff_df$i31 - 4),
  i5_na = abs(neg_aff_df$i46 -4),
  i1_sr = self_repr_df$i6,
  i2_sr = self_repr_df$i21,
  i3_sr = self_repr_df$i26,
  i4_sr = self_repr_df$i36,
  i5_sr = self_repr_df$i41,
  i6_sr = self_repr_df$i51,
  i7_sr = self_repr_df$i56,
  self_reproach,
  extraversion = NA,
  openness = NA,
  agreeableness = NA,
  nonantagonistic_orientation = NA, 
  prosocial_orientation = NA,
  conscientiousness = NA,
  # COPE
  cope_nvi = NA,
  social_support = NA,
  avoiding_strategies = NA,
  positive_attitude,
  problem_orientation,
  transcendent_orientation = NA,
  avoiding_strategies_R = NA, # reversed
  transcendent_orientation_R= NA, # reversed
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
saveRDS(thedat, file = here("data", "processed", "psicometria_sample.Rds"))

# write.table(
#   thedat,
#   file = here("data", "processed", "psicometria_sample.csv"),
#   sep = ";",
#   row.names = FALSE
# )


# eof  ---




