
# Items are never reversed.


# Rescue workers sample ---------------------------------------------------


get_items_rescue_workers <- function() {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  library("forcats")
  library("readxl")
  library("mice")
  
  df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
  df_toscana$where <- "toscana"
  df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
  df_lombardia$where <- "lombardia"
  
  # Merge the two data.frames 
  df <- rbind(df_toscana, df_lombardia)
  
  # Remove subjects with only NAs.
  df <- df[!is.na(df$X17), ]
  
  # Rename variables
  df <- df %>% 
    rename(
      date = `Informazioni cronologiche`,
      permission = `Trattamento dei dati personali`,
      id = `Inserisci il codice identificativo come sopra indicato:`,
      gender = Sesso,
      education = `Livello istruzione`,
      employment =  Occupazione,
      is_rescue_worker = `Sei un Soccorritore in Emergenza?`,
      red_cross_commeetee_location =  `Comitato Croce Rossa Italiana di appartenenza (per es. Comitato di Firenze)`,
      rescue_worker_qualification =  `Possiedi la qualifica TSSA/PTSI?`, 
      last_training = `Quando hai effettuato l'ultimo re-training, corso, inerente la qualifica TSSA/FULL-D?`,
      rate_of_activity =  `Con quale frequenza fai turno in Ambulanza?`,
      job_qualification =  `Generalmente quale ruolo ricopri nella squadra?`,
      is_job_qualification_invariant= `Tendi a ricoprire sempre lo stesso ruolo?`,
      is_team_invariant = `Generalmente fai turno sempre con la stessa squadra?`,
      age =  `Quanti anni hai?`
    ) 
  
  # The data.frame contains the following scales:
  
  # - NEO_FFI_60: 60 items, from X17 to X76
  # - COPE_NVI: 60 items, from X77 to X136
  # - PTGI: 29 items, X137 to X144, X145 to X165
  # - IES_R: 28 items, items X166 to X171, X172 to X193
  # - SCS: 26 items, items X194 to 219
  # - MSPSS: 12 items, items X220 to 231
  
  # Imputation ----
  
  # First imputation of age; otherwise the second imputation does not work.
  subset_vars <- c(
    "gender",                      "age",                                              
    "education",                   "employment",                                       
    "is_rescue_worker",            "red_cross_commeetee_location",                     
    "rescue_worker_qualification", "Anni di esperienza TSSA/PTSI (inserire il numero)",
    "last_training",               "rate_of_activity",                                 
    "job_qualification",           "is_job_qualification_invariant",                   
    "is_team_invariant"
  )
  
  foo1 <- df %>% 
    dplyr::select(all_of(subset_vars))
  
  foo2 <- foo1
  fake_names <- paste0("fake_name_", 1:ncol(foo1))
  names(foo2) <- fake_names
  set.seed(12345)
  imp <- mice(foo2, meth = 'pmm', m = 1, maxit = 5)
  completed_data <- complete(imp, 1)
  # Replace fake columns' names with original columns names.
  names(completed_data) <- names(foo1)
  foo1 <- completed_data
  
  # Replace the imputed age in the df data.frame.
  df$age <- foo1$age
  
  temp <- df
  fake_names <- paste0("fake_name_", 1:ncol(df))
  names(temp) <- fake_names
  set.seed(12345)
  imp <- mice(temp, meth = 'pmm', m = 1, maxit = 5)
  completed_data <- complete(imp, 1)
  # Replace fake columns' names with original columns names.
  names(completed_data) <- names(df)
  df <- completed_data
  
  # NEO-FIve Factor Inventory (NEO-FFI) 
  neo_ffi_60 <- df %>% 
    dplyr::select(X17:X76)
  item_names <- paste0("neoffi_", 1:60)
  names(neo_ffi_60) <- item_names
  
  # Coping Orientation to Problems Experienced (COPE-NVI) 
  cope_nvi <- df %>% 
    select(X77:X136)
  item_names <- paste0("cope_", 1:60)
  names(cope_nvi) <- item_names
  
  # Post Traumatic Growth Inventory (PTGI) 
  ptgi <- df %>% 
    select(X145:X165)
  item_names <- paste0("ptgi_", 1:21)
  names(ptgi) <- item_names
  
  # Impact of Event Scale - Revised (IES-R) 
  ies_r <- df %>% 
    select(X172:X193)
  item_names <- paste0("ies_", 1:22)
  names(ies_r) <- item_names
  
  # Self-Compassion Scale (SCS) 
  scs <- df %>% 
    select(X194:X219)
  item_names <- paste0("scs_", 1:26)
  names(scs) <- item_names
  
  # Multidimensional Scale of Perceived Social Support (MSPSS) 
  mspss <- df %>% 
    select(X220:X231)
  item_names <- paste0("mspss_", 1:12)
  names(mspss) <- item_names
  
  # Create data.frame with all items of rescue workers,
  rescue_workers_all_items <- data.frame(
    neo_ffi_60, 
    cope_nvi,
    ptgi,
    ies_r,
    scs,
    mspss
  )
  
  df1 <- df %>% 
    rename(
      years_of_experience = "Anni di esperienza TSSA/PTSI (inserire il numero)"
    ) 
  
  # Add demographic information.
  # This was the minimal set common to all samples.
  # rescue_workers_all_items$age <- df$age
  # rescue_workers_all_items$gender <- df$gender
  # rescue_workers_all_items$is_rescue_worker <- df$is_rescue_worker
  # Now I will add ALL the information available, even if is not present
  # in the other samples.
  
  # [1] "date"                                             
  # [2] "permission"                                       
  # [3] "id"                                               
  # [4] "gender"                                           
  # [5] "age"                                              
  # [6] "education"                                        
  # [7] "employment"                                       
  # [8] "is_rescue_worker"                                 
  # [9] "red_cross_commeetee_location"                     
  # [10] "rescue_worker_qualification"                      
  # [11] "Anni di esperienza TSSA/PTSI (inserire il numero)"
  # [12] "last_training"                                    
  # [13] "rate_of_activity"                                 
  # [14] "job_qualification"                                
  # [15] "is_job_qualification_invariant"                   
  # [16] "is_team_invariant
  
  rescue_workers_all_items$date <- df$date
  rescue_workers_all_items$id <- df$id
  rescue_workers_all_items$gender <- df$gender
  rescue_workers_all_items$age <- df$age
  rescue_workers_all_items$education <- df$education
  rescue_workers_all_items$employment <- df$employment
  rescue_workers_all_items$is_rescue_worker <- df$is_rescue_worker
  rescue_workers_all_items$red_cross_commeetee_location <- df$red_cross_commeetee_location
  rescue_workers_all_items$rescue_worker_qualification <- df$rescue_worker_qualification
  rescue_workers_all_items$years_of_experience <- df$years_of_experience
  rescue_workers_all_items$last_training <- df$last_training
  rescue_workers_all_items$rate_of_activity <- df$rate_of_activity
  rescue_workers_all_items$job_qualification <- df$job_qualification
  rescue_workers_all_items$is_job_qualification_invariant <- df$is_job_qualification_invariant
  rescue_workers_all_items$is_team_invariant <- df$is_team_invariant
 
  
  rescue_workers_all_items
}



# Community sample --------------------------------------------------------

get_items_community_sample <- function() {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  library("forcats")
  library("readxl")
  library("mice")
  
  options(max.print=999999)
  
  # Read data
  df <- read_excel(here("data", "raw", "non_soccorritori_20210404.xlsx"))
  
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
  
  # Imputation 
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
  
  # NEO-FIve Factor Inventory (NEO-FFI)
  neo_ffi_60 <- df[, 9:68]
  item_names <- paste0("neoffi_", 1:60)
  names(neo_ffi_60) <- item_names
  
  # Coping Orientation to Problems Experienced (COPE-NVI
  cope_nvi <- df[, 69:128]
  item_names <- paste0("cope_", 1:60)
  names(cope_nvi) <- item_names
  
  # Post Traumatic Growth Inventory (PTGI) 
  ptgi <- df[, 131:151]
  item_names <- paste0("ptgi_", 1:21)
  names(ptgi) <- item_names
  
  # Impact of Event Scale - Revised (IES-R)
  ies_r <- df[, 154:175]
  item_names <- paste0("ies_", 1:22)
  names(ies_r) <- item_names
  
  # Self-Compassion Scale (SCS) 
  scs <- df[, 176:201]
  item_names <- paste0("scs_", 1:26)
  names(scs) <- item_names
  
  # Multidimensional Scale of Perceived Social Support (MSPSS) 
  mspss <- df[, 202:213]
  item_names <- paste0("mspss_", 1:12)
  names(mspss) <- item_names
  
  # Create data.frame with all items of community sample.
  community_sample_all_items <- data.frame(
    neo_ffi_60, 
    cope_nvi,
    ptgi,
    ies_r,
    scs,
    mspss
  )
  
  # Add demographic information.
  community_sample_all_items$age <- df$age
  community_sample_all_items$gender <- df$gender
  community_sample_all_items$is_rescue_worker <- "No"
  community_sample_all_items$id <- df$id
  community_sample_all_items$education <- df$education
  community_sample_all_items$employment <- df$employment
  community_sample_all_items$is_rescue_worker <- "no"
  
  community_sample_all_items
}




# Students sample ---------------------------------------------------------


get_items_student_sample <- function() {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse")) 
  library("forcats")
  library("readxl")
  library("mice")
  library("careless")
  library("EnvStats")
  
  options(max.print=999999)
  
  source(here("src", "R", "functions", "self_compassion_fnc.R"))
  
  # Read data
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
  
  # Imputation 
  temp <- df
  fake_names <- paste0("fake_name_", 1:ncol(df))
  names(temp) <- fake_names
  set.seed(12345)
  imp <- mice(temp, meth = 'pmm', m = 1, maxit = 5)
  completed_data <- complete(imp, 1)
  # Replace fake columns' names with original columns names.
  names(completed_data) <- names(df)
  df <- completed_data
  
  # NEO-Five Factor Inventory (NEO-FFI) 
  # Negative Affect subscale.
  neg_aff_columns <- c(36, 38, 39, 42, 45)
  neg_aff_df <- df[, neg_aff_columns]
  neg_aff_item_names <- c("neoffi_1", "neoffi_11", "neoffi_16", "neoffi_31", 
                          "neoffi_46")
  names(neg_aff_df) <- neg_aff_item_names
  # Self Reproach subscale.
  self_repr_columns <- c(37, 40, 41, 43, 44, 46, 47)
  self_repr_df <- df[, self_repr_columns]
  self_repr_item_names <- c("neoffi_6", "neoffi_21", "neoffi_26", "neoffi_36", 
                            "neoffi_41", "neoffi_51", "neoffi_56")
  names(self_repr_df) <- self_repr_item_names
  neo_ffi_60 <- bind_cols(neg_aff_df, self_repr_df)
  
  # Coping Orientation to Problems Experienced (COPE-NVI)
  cope_nvi <- df[, 71:94]
  # Positive attitude subscale.
  pos_att_columns <- c(71, 73,  74,  77, 78,  80,  83,  85, 87,  89,  90, 94)
  pos_att_df <- df[, pos_att_columns]
  pos_att_item_names <- c("cope_1", "cope_10", "cope_13", "cope_21", "cope_22", 
                          "cope_29", "cope_38", "cope_41", "cope_44", "cope_49", 
                          "cope_54", "cope_59") 
  names(pos_att_df) <- pos_att_item_names
  # Problem orientation subscale.
  prob_or_columns <- c(72, 75,  76,  79, 81,  82,  84,  86, 88,  91,  92, 93)
  prob_or_df <- df[, prob_or_columns]
  prob_or_item_names <- c("cope_5", "cope_15", "cope_19", "cope_25", "cope_32", 
                          "cope_33", "cope_39", "cope_42", "cope_47", "cope_55", 
                          "cope_56", "cope_58") 
  names(prob_or_df) <- prob_or_item_names
  cope <- bind_cols(pos_att_df, prob_or_df)
  
  # Post Traumatic Growth Inventory (PTGI) 
  ptgi <- df[, 96:116]
  item_names <- paste0("ptgi_", 1:21)
  names(ptgi) <- item_names
  
  # Impact of Event Scale - Revised (IES-R)
  ies_r <- df[, 49:70]
  item_names <- paste0("ies_", 1:22)
  names(ies_r) <- item_names
  
  # Self-Compassion Scale (SCS) 
  scs <- df[, 224:249]
  item_names <- paste0("scs_", 1:26)
  names(scs) <- item_names
  
  # Multidimensional Scale of Perceived Social Support (MSPSS) ----
  mspss <- df[, 117:128]
  item_names <- paste0("mspss_", 1:12)
  names(mspss) <- item_names
  
  # Create data.frame with all items of community sample.
  students_sample_all_items <- data.frame(
    neo_ffi_60, 
    cope,
    ptgi,
    ies_r,
    scs,
    mspss
  )
  
  # Add demographic information.
  students_sample_all_items$date <- df$date
  students_sample_all_items$id <- df$id
  students_sample_all_items$age <- df$age
  students_sample_all_items$gender <- df$gender
  students_sample_all_items$is_rescue_worker <-  
    df$`Svolgi attività di volontariato come soccorritore di Emergenza?`
  students_sample_all_items$is_married <- df$"Stato civile"
  students_sample_all_items$education <- df$education
  students_sample_all_items$employment <- df$employment
    
  students_sample_all_items
}


