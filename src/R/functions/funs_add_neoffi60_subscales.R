#' Add NEO-FFI-60 subscales.
add_neoffi60_subscales <- function(all_items) {
  
  # Neuroticism ---
  
  # Negative affect.
  all_items$negative_affect <- 
    item.reverse(all_items$neoffi_1, min = 0, max = 4) + 
    all_items$neoffi_11 + 
    item.reverse(all_items$neoffi_16, min = 0, max = 4) +
    item.reverse(all_items$neoffi_31, min = 0, max = 4) +
    item.reverse(all_items$neoffi_46, min = 0, max = 4) 
  
  # Self reproach.
  all_items$self_reproach <- 
    all_items$neoffi_6 + all_items$neoffi_21 + all_items$neoffi_26 + 
    all_items$neoffi_36 + all_items$neoffi_41 + all_items$neoffi_51 + 
    all_items$neoffi_56
  
  # Extraversion ---
  
  all_items$positive_affect <- 
    all_items$neoffi_7 +
    item.reverse(all_items$neoffi_12, min = 0, max = 4) +
    all_items$neoffi_37 +
    item.reverse(all_items$neoffi_42, min = 0, max = 4)
  
  all_items$sociability <- 
    all_items$neoffi_2 +
    all_items$neoffi_17 +
    item.reverse(all_items$neoffi_27, min = 0, max = 4) +
    item.reverse(all_items$neoffi_57, min = 0, max = 4)
  
  all_items$activity <- 
    all_items$neoffi_22 +
    all_items$neoffi_32 +
    all_items$neoffi_47 +
    all_items$neoffi_52
  
  # Openness ---
  
  # aesthetic_interests
  all_items$aesthetic_interests <- 
    all_items$neoffi_13 +
    item.reverse(all_items$neoffi_23, min = 0, max = 4) +
    all_items$neoffi_43
  
  # intellectual_interests
  all_items$intellectual_interests <- 
    item.reverse(all_items$neoffi_48, min = 0, max = 4) +
    all_items$neoffi_53 +
    all_items$neoffi_58
  
  # Unconventionality
  all_items$unconventionality <- 
    item.reverse(all_items$neoffi_3, min = 0, max = 4) +
    item.reverse(all_items$neoffi_8, min = 0, max = 4) +
    item.reverse(all_items$neoffi_18, min = 0, max = 4) +
    item.reverse(all_items$neoffi_38, min = 0, max = 4) 
  
  # Agreeableness ---
  
  # Nonantagonistic Orientation
  all_items$nonantagonistic_orientation <- 
    item.reverse(all_items$neoffi_9, min = 0, max = 4) +
    item.reverse(all_items$neoffi_14, min = 0, max = 4) +
    all_items$neoffi_19 +
    item.reverse(all_items$neoffi_24, min = 0, max = 4) +
    item.reverse(all_items$neoffi_29, min = 0, max = 4) +
    item.reverse(all_items$neoffi_44, min = 0, max = 4) +
    item.reverse(all_items$neoffi_54, min = 0, max = 4) +
    item.reverse(all_items$neoffi_59, min = 0, max = 4)
  
  # Prosocial Orientation
  all_items$prosocial_orientation <- 
    all_items$neoffi_4 +
    all_items$neoffi_34 +
    item.reverse(all_items$neoffi_39, min = 0, max = 4) +
    all_items$neoffi_49
  
  # Conscientiousness ---
  
  # Orderliness
  all_items$orderliness <- 
    all_items$neoffi_5 +
    all_items$neoffi_10 +
    item.reverse(all_items$neoffi_15, min = 0, max = 4) +
    item.reverse(all_items$neoffi_30, min = 0, max = 4) +
    item.reverse(all_items$neoffi_55, min = 0, max = 4) 
  
  # Goal-Striving
  all_items$goal_striving <- 
    all_items$neoffi_25 +
    all_items$neoffi_35 +
    all_items$neoffi_60
  
  # Dependability
  all_items$dependability <- 
    all_items$neoffi_20 +
    all_items$neoffi_40 +
    item.reverse(all_items$neoffi_45, min = 0, max = 4) +
    all_items$neoffi_40
  
  all_items
}