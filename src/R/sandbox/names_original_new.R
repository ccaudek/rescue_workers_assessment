> sort(names_original)
"activity_rate"                  "age_imp"                       
"anomaly_score"                  "avoiding"                      
"avoiding_strategies"            "common_humanity"               
"edu"                            "employment"                    
"family"                         "friends"                       
"gender"                         "id"                            
"interpersonal_relationships"    "intrusivity"                   
"iperarousal"                    "is_job_qualification_invariant"
"is_team_invariant"              "isolation"                     
"job_qualification"              "last_training"                 
"life_appreciation"              "mindfulness"                   
"negative_affect"                "new_possibilities"             
"outlier"                        "over_identification"           
"personal_strength"              "positive_attitude"             
"problem_orientation"            "rate_of_activity"              
"red_cross_commeetee_location"   "rescue_worker_qualification"   
"self_judgment"                  "self_kindness"                 
"self_reproach"                  "significant_other"             
"social_support"                 "spirituality_changes"          
"training_time"                  "transcendent_orientation"      
"where"                          "years_experience"              


# all_items

> sort(new_names)
[1] "age"                      "agreeableness"            "appreciation_of_life"    
[4] "avoiding"                 "avoiding_strategies"      "common_humanity"         
[7] "conscientiousness"        "cope_1"                   "cope_10"                 
[10] "cope_total_score"         "extraversion"             "family"                  
[13] "friends"                  "gender"                   "group"                   
[16] "hyperarousal"             "id"                       "ies_ts"                  
[19] "intrusivity"              "is_rescue_worker"         "isolation"               
[22] "mindfulness"              "mpss_tot"                 "neg_self_compassion"     
[25] "neuroticism"              "new_possibilities"        "openness"                
[28] "over_identification"      "personal_strength"        "pos_self_compassion"     
[31] "positive_attitude"        "problem_orientation"      "ptgi_total_score"        
[34] "relating_to_others"       "self_judgment"            "self_kindness"           
[37] "significant_other"        "social_support"           "spirituality"            
[40] "transcendent_orientation"

> setdiff(names_original, new_names) %>% sort()
[1] "activity_rate"                  "age_imp"                       
[3] "anomaly_score"                  "edu"                           
[5] "employment"                     "interpersonal_relationships"   
[7] "iperarousal"                    "is_job_qualification_invariant"
[9] "is_team_invariant"              "job_qualification"             
[11] "last_training"                  "life_appreciation"             
[13] "negative_affect"                "outlier"                       
[15] "rate_of_activity"               "red_cross_commeetee_location"  
[17] "rescue_worker_qualification"    "self_reproach"                 
[19] "spirituality_changes"           "training_time"                 
[21] "where"                          "years_experience"   


# Problems
