#Survivor Winner Model Optimize

#Read in packages
library('dplyr')
library('h2o')
library('stringr')
library('ggplot2')
library('tidyr')

options(scipen = 100)

# ------------------------------   STACK RESULTS   ------------------------------

#Read in dataset
player_dataset = read.csv("Survivor_Players_Dataset.csv", header = T, stringsAsFactors = T)
player_dataset$FirstName = as.character(player_dataset$FirstName)
player_dataset$LastName = as.character(player_dataset$LastName)
player_dataset$Nickname = as.character(player_dataset$Nickname)
player_dataset$Name = as.character(player_dataset$Name)
player_dataset[player_dataset==""]=NA
ModelingDS = player_dataset %>% filter(SeasonNumber <= 40)

#Build Variables
#Week 1
ModelingDS$week1_avg_vis = ModelingDS$week1_visibility
ModelingDS$week1_tone_p = ifelse(ModelingDS$week1_tone == "P",1,0)
ModelingDS$week1_tone_n = ifelse(ModelingDS$week1_tone == "N",1,0)
ModelingDS$week1_tone_m = ifelse(ModelingDS$week1_tone == "M",1,0)
ModelingDS$week1_tone_any = ModelingDS$week1_tone_p + ModelingDS$week1_tone_n + ModelingDS$week1_tone_m
ModelingDS$week1_rating_INV = ifelse(ModelingDS$week1_rating == "INV",1,0)
ModelingDS$week1_rating_UTR = ifelse(ModelingDS$week1_rating == "UTR",1,0)
ModelingDS$week1_rating_MOR = ifelse(ModelingDS$week1_rating == "MOR",1,0)
ModelingDS$week1_rating_CP = ifelse(ModelingDS$week1_rating == "CP",1,0)
ModelingDS$week1_rating_OTT = ifelse(ModelingDS$week1_rating == "OTT",1,0)
#Week 2
ModelingDS$week2_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility)/2
ModelingDS$week2_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility)/3
ModelingDS$week2_tone_p = ifelse(ModelingDS$week2_tone == "P",1,0) + ModelingDS$week1_tone_p
ModelingDS$week2_tone_n = ifelse(ModelingDS$week2_tone == "N",1,0) + ModelingDS$week1_tone_n
ModelingDS$week2_tone_m = ifelse(ModelingDS$week2_tone == "M",1,0) + ModelingDS$week1_tone_m
ModelingDS$week2_tone_any = ModelingDS$week2_tone_p + ModelingDS$week2_tone_n + ModelingDS$week2_tone_m
ModelingDS$week2_rating_INV = ifelse(ModelingDS$week2_rating == "INV",1,0) + ModelingDS$week1_rating_INV
ModelingDS$week2_rating_UTR = ifelse(ModelingDS$week2_rating == "UTR",1,0) + ModelingDS$week1_rating_UTR
ModelingDS$week2_rating_MOR = ifelse(ModelingDS$week2_rating == "MOR",1,0) + ModelingDS$week1_rating_MOR
ModelingDS$week2_rating_CP = ifelse(ModelingDS$week2_rating == "CP",1,0) + ModelingDS$week1_rating_CP
ModelingDS$week2_rating_OTT = ifelse(ModelingDS$week2_rating == "OTT",1,0) + ModelingDS$week1_rating_OTT
#Week 3
ModelingDS$week3_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility)/3
ModelingDS$week3_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility)/6
ModelingDS$week3_mavg_vis = (ModelingDS$week1_visibility + 
                               ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility)/3
ModelingDS$week3_tone_p = ifelse(ModelingDS$week3_tone == "P",1,0) + ModelingDS$week2_tone_p
ModelingDS$week3_tone_n = ifelse(ModelingDS$week3_tone == "N",1,0) + ModelingDS$week2_tone_n
ModelingDS$week3_tone_m = ifelse(ModelingDS$week3_tone == "M",1,0) + ModelingDS$week2_tone_m
ModelingDS$week3_tone_any = ModelingDS$week3_tone_p + ModelingDS$week3_tone_n + ModelingDS$week3_tone_m
ModelingDS$week3_tone_p_chg = ModelingDS$week3_tone_p
ModelingDS$week3_tone_n_chg = ModelingDS$week3_tone_n
ModelingDS$week3_tone_m_chg = ModelingDS$week3_tone_m
ModelingDS$week3_tone_any_chg = ModelingDS$week3_tone_any
ModelingDS$week3_rating_INV = ifelse(ModelingDS$week3_rating == "INV",1,0) + ModelingDS$week2_rating_INV
ModelingDS$week3_rating_UTR = ifelse(ModelingDS$week3_rating == "UTR",1,0) + ModelingDS$week2_rating_UTR
ModelingDS$week3_rating_MOR = ifelse(ModelingDS$week3_rating == "MOR",1,0) + ModelingDS$week2_rating_MOR
ModelingDS$week3_rating_CP = ifelse(ModelingDS$week3_rating == "CP",1,0) + ModelingDS$week2_rating_CP
ModelingDS$week3_rating_OTT = ifelse(ModelingDS$week3_rating == "OTT",1,0) + ModelingDS$week2_rating_OT
ModelingDS$Week3_rating_INV_chg = ModelingDS$week3_rating_INV
ModelingDS$Week3_rating_UTR_chg = ModelingDS$week3_rating_UTR
ModelingDS$Week3_rating_MOR_chg = ModelingDS$week3_rating_MOR
ModelingDS$Week3_rating_CP_chg = ModelingDS$week3_rating_CP
ModelingDS$Week3_rating_OTT_chg = ModelingDS$week3_rating_OTT
#Week 4
ModelingDS$week4_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility)/4
ModelingDS$week4_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility)/10
ModelingDS$week4_mavg_vis = (ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility)/3
ModelingDS$week4_tone_p = ifelse(ModelingDS$week4_tone == "P",1,0) + ModelingDS$week3_tone_p
ModelingDS$week4_tone_n = ifelse(ModelingDS$week4_tone == "N",1,0) + ModelingDS$week3_tone_n
ModelingDS$week4_tone_m = ifelse(ModelingDS$week4_tone == "M",1,0) + ModelingDS$week3_tone_m
ModelingDS$week4_tone_any = ModelingDS$week4_tone_p + ModelingDS$week4_tone_n + ModelingDS$week4_tone_m
ModelingDS$week4_tone_p_chg = ModelingDS$week4_tone_p - ModelingDS$week1_tone_p
ModelingDS$week4_tone_n_chg = ModelingDS$week4_tone_n - ModelingDS$week1_tone_n
ModelingDS$week4_tone_m_chg = ModelingDS$week4_tone_m - ModelingDS$week1_tone_m
ModelingDS$week4_tone_any_chg = ModelingDS$week4_tone_any - ModelingDS$week1_tone_any
ModelingDS$week4_rating_INV = ifelse(ModelingDS$week4_rating == "INV",1,0) + ModelingDS$week3_rating_INV
ModelingDS$week4_rating_UTR = ifelse(ModelingDS$week4_rating == "UTR",1,0) + ModelingDS$week3_rating_UTR
ModelingDS$week4_rating_MOR = ifelse(ModelingDS$week4_rating == "MOR",1,0) + ModelingDS$week3_rating_MOR
ModelingDS$week4_rating_CP = ifelse(ModelingDS$week4_rating == "CP",1,0) + ModelingDS$week3_rating_CP
ModelingDS$week4_rating_OTT = ifelse(ModelingDS$week4_rating == "OTT",1,0) + ModelingDS$week3_rating_OT
ModelingDS$Week4_rating_INV_chg = ModelingDS$week4_rating_INV - ModelingDS$week1_rating_INV
ModelingDS$Week4_rating_UTR_chg = ModelingDS$week4_rating_UTR - ModelingDS$week1_rating_UTR
ModelingDS$Week4_rating_MOR_chg = ModelingDS$week4_rating_MOR - ModelingDS$week1_rating_MOR
ModelingDS$Week4_rating_CP_chg = ModelingDS$week4_rating_CP - ModelingDS$week1_rating_CP
ModelingDS$Week4_rating_OTT_chg = ModelingDS$week4_rating_OTT - ModelingDS$week1_rating_OTT
#Week 5
ModelingDS$week5_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility +
                              ModelingDS$week5_visibility)/5
ModelingDS$week5_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility +
                               5*ModelingDS$week5_visibility)/15
ModelingDS$week5_mavg_vis = (ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility + 
                               ModelingDS$week5_visibility)/3
ModelingDS$week5_tone_p = ifelse(ModelingDS$week5_tone == "P",1,0) + ModelingDS$week4_tone_p
ModelingDS$week5_tone_n = ifelse(ModelingDS$week5_tone == "N",1,0) + ModelingDS$week4_tone_n
ModelingDS$week5_tone_m = ifelse(ModelingDS$week5_tone == "M",1,0) + ModelingDS$week4_tone_m
ModelingDS$week5_tone_any = ModelingDS$week5_tone_p + ModelingDS$week5_tone_n + ModelingDS$week5_tone_m
ModelingDS$week5_tone_p_chg = ModelingDS$week5_tone_p - ModelingDS$week2_tone_p
ModelingDS$week5_tone_n_chg = ModelingDS$week5_tone_n - ModelingDS$week2_tone_n
ModelingDS$week5_tone_m_chg = ModelingDS$week5_tone_m - ModelingDS$week2_tone_m
ModelingDS$week5_tone_any_chg = ModelingDS$week5_tone_any - ModelingDS$week2_tone_any
ModelingDS$week5_rating_INV = ifelse(ModelingDS$week5_rating == "INV",1,0) + ModelingDS$week4_rating_INV
ModelingDS$week5_rating_UTR = ifelse(ModelingDS$week5_rating == "UTR",1,0) + ModelingDS$week4_rating_UTR
ModelingDS$week5_rating_MOR = ifelse(ModelingDS$week5_rating == "MOR",1,0) + ModelingDS$week4_rating_MOR
ModelingDS$week5_rating_CP = ifelse(ModelingDS$week5_rating == "CP",1,0) + ModelingDS$week4_rating_CP
ModelingDS$week5_rating_OTT = ifelse(ModelingDS$week5_rating == "OTT",1,0) + ModelingDS$week4_rating_OT
ModelingDS$Week5_rating_INV_chg = ModelingDS$week5_rating_INV - ModelingDS$week2_rating_INV
ModelingDS$Week5_rating_UTR_chg = ModelingDS$week5_rating_UTR - ModelingDS$week2_rating_UTR
ModelingDS$Week5_rating_MOR_chg = ModelingDS$week5_rating_MOR - ModelingDS$week2_rating_MOR
ModelingDS$Week5_rating_CP_chg = ModelingDS$week5_rating_CP - ModelingDS$week2_rating_CP
ModelingDS$Week5_rating_OTT_chg = ModelingDS$week5_rating_OTT - ModelingDS$week2_rating_OTT
#Week 6 
ModelingDS$week6_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility +
                              ModelingDS$week5_visibility +
                              ModelingDS$week6_visibility)/6
ModelingDS$week6_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility +
                               5*ModelingDS$week5_visibility +
                               6*ModelingDS$week6_visibility)/21
ModelingDS$week6_mavg_vis = (ModelingDS$week4_visibility + 
                               ModelingDS$week5_visibility + 
                               ModelingDS$week6_visibility)/3
ModelingDS$week6_tone_p = ifelse(ModelingDS$week6_tone == "P",1,0) + ModelingDS$week5_tone_p
ModelingDS$week6_tone_n = ifelse(ModelingDS$week6_tone == "N",1,0) + ModelingDS$week5_tone_n
ModelingDS$week6_tone_m = ifelse(ModelingDS$week6_tone == "M",1,0) + ModelingDS$week5_tone_m
ModelingDS$week6_tone_any = ModelingDS$week6_tone_p + ModelingDS$week6_tone_n + ModelingDS$week6_tone_m
ModelingDS$week6_tone_p_chg = ModelingDS$week6_tone_p - ModelingDS$week3_tone_p
ModelingDS$week6_tone_n_chg = ModelingDS$week6_tone_n - ModelingDS$week3_tone_n
ModelingDS$week6_tone_m_chg = ModelingDS$week6_tone_m - ModelingDS$week3_tone_m
ModelingDS$week6_tone_any_chg = ModelingDS$week6_tone_any - ModelingDS$week3_tone_any
ModelingDS$week6_rating_INV = ifelse(ModelingDS$week6_rating == "INV",1,0) + ModelingDS$week5_rating_INV
ModelingDS$week6_rating_UTR = ifelse(ModelingDS$week6_rating == "UTR",1,0) + ModelingDS$week5_rating_UTR
ModelingDS$week6_rating_MOR = ifelse(ModelingDS$week6_rating == "MOR",1,0) + ModelingDS$week5_rating_MOR
ModelingDS$week6_rating_CP = ifelse(ModelingDS$week6_rating == "CP",1,0) + ModelingDS$week5_rating_CP
ModelingDS$week6_rating_OTT = ifelse(ModelingDS$week6_rating == "OTT",1,0) + ModelingDS$week5_rating_OT
ModelingDS$Week6_rating_INV_chg = ModelingDS$week6_rating_INV - ModelingDS$week3_rating_INV
ModelingDS$Week6_rating_UTR_chg = ModelingDS$week6_rating_UTR - ModelingDS$week3_rating_UTR
ModelingDS$Week6_rating_MOR_chg = ModelingDS$week6_rating_MOR - ModelingDS$week3_rating_MOR
ModelingDS$Week6_rating_CP_chg = ModelingDS$week6_rating_CP - ModelingDS$week3_rating_CP
ModelingDS$Week6_rating_OTT_chg = ModelingDS$week6_rating_OTT - ModelingDS$week3_rating_OTT
#Week 7
ModelingDS$week7_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility +
                              ModelingDS$week5_visibility +
                              ModelingDS$week6_visibility +
                              ModelingDS$week7_visibility)/7
ModelingDS$week7_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility +
                               5*ModelingDS$week5_visibility +
                               6*ModelingDS$week6_visibility +
                               7*ModelingDS$week7_visibility)/28
ModelingDS$week7_mavg_vis = (ModelingDS$week5_visibility + 
                               ModelingDS$week6_visibility + 
                               ModelingDS$week7_visibility)/3
ModelingDS$week7_tone_p = ifelse(ModelingDS$week7_tone == "P",1,0) + ModelingDS$week6_tone_p
ModelingDS$week7_tone_n = ifelse(ModelingDS$week7_tone == "N",1,0) + ModelingDS$week6_tone_n
ModelingDS$week7_tone_m = ifelse(ModelingDS$week7_tone == "M",1,0) + ModelingDS$week6_tone_m
ModelingDS$week7_tone_any = ModelingDS$week7_tone_p + ModelingDS$week7_tone_n + ModelingDS$week7_tone_m
ModelingDS$week7_tone_p_chg = ModelingDS$week7_tone_p - ModelingDS$week4_tone_p
ModelingDS$week7_tone_n_chg = ModelingDS$week7_tone_n - ModelingDS$week4_tone_n
ModelingDS$week7_tone_m_chg = ModelingDS$week7_tone_m - ModelingDS$week4_tone_m
ModelingDS$week7_tone_any_chg = ModelingDS$week7_tone_any - ModelingDS$week4_tone_any
ModelingDS$week7_rating_INV = ifelse(ModelingDS$week7_rating == "INV",1,0) + ModelingDS$week6_rating_INV
ModelingDS$week7_rating_UTR = ifelse(ModelingDS$week7_rating == "UTR",1,0) + ModelingDS$week6_rating_UTR
ModelingDS$week7_rating_MOR = ifelse(ModelingDS$week7_rating == "MOR",1,0) + ModelingDS$week6_rating_MOR
ModelingDS$week7_rating_CP = ifelse(ModelingDS$week7_rating == "CP",1,0) + ModelingDS$week6_rating_CP
ModelingDS$week7_rating_OTT = ifelse(ModelingDS$week7_rating == "OTT",1,0) + ModelingDS$week6_rating_OT
ModelingDS$Week7_rating_INV_chg = ModelingDS$week7_rating_INV - ModelingDS$week4_rating_INV
ModelingDS$Week7_rating_UTR_chg = ModelingDS$week7_rating_UTR - ModelingDS$week4_rating_UTR
ModelingDS$Week7_rating_MOR_chg = ModelingDS$week7_rating_MOR - ModelingDS$week4_rating_MOR
ModelingDS$Week7_rating_CP_chg = ModelingDS$week7_rating_CP - ModelingDS$week4_rating_CP
ModelingDS$Week7_rating_OTT_chg = ModelingDS$week7_rating_OTT - ModelingDS$week4_rating_OTT
#Week 8
ModelingDS$week8_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility +
                              ModelingDS$week5_visibility +
                              ModelingDS$week6_visibility +
                              ModelingDS$week7_visibility +
                              ModelingDS$week8_visibility)/8
ModelingDS$week8_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility +
                               5*ModelingDS$week5_visibility +
                               6*ModelingDS$week6_visibility +
                               7*ModelingDS$week7_visibility +
                               8*ModelingDS$week8_visibility)/36
ModelingDS$week8_mavg_vis = (ModelingDS$week6_visibility + 
                               ModelingDS$week7_visibility + 
                               ModelingDS$week8_visibility)/3
ModelingDS$week8_tone_p = ifelse(ModelingDS$week8_tone == "P",1,0) + ModelingDS$week7_tone_p
ModelingDS$week8_tone_n = ifelse(ModelingDS$week8_tone == "N",1,0) + ModelingDS$week7_tone_n
ModelingDS$week8_tone_m = ifelse(ModelingDS$week8_tone == "M",1,0) + ModelingDS$week7_tone_m
ModelingDS$week8_tone_any = ModelingDS$week8_tone_p + ModelingDS$week8_tone_n + ModelingDS$week8_tone_m
ModelingDS$week8_tone_p_chg = ModelingDS$week8_tone_p - ModelingDS$week5_tone_p
ModelingDS$week8_tone_n_chg = ModelingDS$week8_tone_n - ModelingDS$week5_tone_n
ModelingDS$week8_tone_m_chg = ModelingDS$week8_tone_m - ModelingDS$week5_tone_m
ModelingDS$week8_tone_any_chg = ModelingDS$week8_tone_any - ModelingDS$week5_tone_any
ModelingDS$week8_rating_INV = ifelse(ModelingDS$week8_rating == "INV",1,0) + ModelingDS$week7_rating_INV
ModelingDS$week8_rating_UTR = ifelse(ModelingDS$week8_rating == "UTR",1,0) + ModelingDS$week7_rating_UTR
ModelingDS$week8_rating_MOR = ifelse(ModelingDS$week8_rating == "MOR",1,0) + ModelingDS$week7_rating_MOR
ModelingDS$week8_rating_CP = ifelse(ModelingDS$week8_rating == "CP",1,0) + ModelingDS$week7_rating_CP
ModelingDS$week8_rating_OTT = ifelse(ModelingDS$week8_rating == "OTT",1,0) + ModelingDS$week7_rating_OT
ModelingDS$Week8_rating_INV_chg = ModelingDS$week8_rating_INV - ModelingDS$week5_rating_INV
ModelingDS$Week8_rating_UTR_chg = ModelingDS$week8_rating_UTR - ModelingDS$week5_rating_UTR
ModelingDS$Week8_rating_MOR_chg = ModelingDS$week8_rating_MOR - ModelingDS$week5_rating_MOR
ModelingDS$Week8_rating_CP_chg = ModelingDS$week8_rating_CP - ModelingDS$week5_rating_CP
ModelingDS$Week8_rating_OTT_chg = ModelingDS$week8_rating_OTT - ModelingDS$week5_rating_OTT
#Week 9
ModelingDS$week9_avg_vis = (ModelingDS$week1_visibility + 
                              ModelingDS$week2_visibility + 
                              ModelingDS$week3_visibility + 
                              ModelingDS$week4_visibility +
                              ModelingDS$week5_visibility +
                              ModelingDS$week6_visibility +
                              ModelingDS$week7_visibility +
                              ModelingDS$week8_visibility +
                              ModelingDS$week9_visibility)/9
ModelingDS$week9_wavg_vis = (ModelingDS$week1_visibility + 
                               2*ModelingDS$week2_visibility + 
                               3*ModelingDS$week3_visibility + 
                               4*ModelingDS$week4_visibility +
                               5*ModelingDS$week5_visibility +
                               6*ModelingDS$week6_visibility +
                               7*ModelingDS$week7_visibility +
                               8*ModelingDS$week8_visibility +
                               9*ModelingDS$week9_visibility)/45
ModelingDS$week9_mavg_vis = (ModelingDS$week7_visibility + 
                               ModelingDS$week8_visibility + 
                               ModelingDS$week9_visibility)/3
ModelingDS$week9_tone_p = ifelse(ModelingDS$week9_tone == "P",1,0) + ModelingDS$week8_tone_p
ModelingDS$week9_tone_n = ifelse(ModelingDS$week9_tone == "N",1,0) + ModelingDS$week8_tone_n
ModelingDS$week9_tone_m = ifelse(ModelingDS$week9_tone == "M",1,0) + ModelingDS$week8_tone_m
ModelingDS$week9_tone_any = ModelingDS$week9_tone_p + ModelingDS$week9_tone_n + ModelingDS$week9_tone_m
ModelingDS$week9_tone_p_chg = ModelingDS$week9_tone_p - ModelingDS$week6_tone_p
ModelingDS$week9_tone_n_chg = ModelingDS$week9_tone_n - ModelingDS$week6_tone_n
ModelingDS$week9_tone_m_chg = ModelingDS$week9_tone_m - ModelingDS$week6_tone_m
ModelingDS$week9_tone_any_chg = ModelingDS$week9_tone_any - ModelingDS$week6_tone_any
ModelingDS$week9_rating_INV = ifelse(ModelingDS$week9_rating == "INV",1,0) + ModelingDS$week8_rating_INV
ModelingDS$week9_rating_UTR = ifelse(ModelingDS$week9_rating == "UTR",1,0) + ModelingDS$week8_rating_UTR
ModelingDS$week9_rating_MOR = ifelse(ModelingDS$week9_rating == "MOR",1,0) + ModelingDS$week8_rating_MOR
ModelingDS$week9_rating_CP = ifelse(ModelingDS$week9_rating == "CP",1,0) + ModelingDS$week8_rating_CP
ModelingDS$week9_rating_OTT = ifelse(ModelingDS$week9_rating == "OTT",1,0) + ModelingDS$week8_rating_OT
ModelingDS$Week9_rating_INV_chg = ModelingDS$week9_rating_INV - ModelingDS$week6_rating_INV
ModelingDS$Week9_rating_UTR_chg = ModelingDS$week9_rating_UTR - ModelingDS$week6_rating_UTR
ModelingDS$Week9_rating_MOR_chg = ModelingDS$week9_rating_MOR - ModelingDS$week6_rating_MOR
ModelingDS$Week9_rating_CP_chg = ModelingDS$week9_rating_CP - ModelingDS$week6_rating_CP
ModelingDS$Week9_rating_OTT_chg = ModelingDS$week9_rating_OTT - ModelingDS$week6_rating_OTT
#Week 10
ModelingDS$week10_avg_vis = (ModelingDS$week1_visibility + 
                               ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility +
                               ModelingDS$week5_visibility +
                               ModelingDS$week6_visibility +
                               ModelingDS$week7_visibility +
                               ModelingDS$week8_visibility +
                               ModelingDS$week9_visibility +
                               ModelingDS$week10_visibility)/10
ModelingDS$week10_wavg_vis = (ModelingDS$week1_visibility + 
                                2*ModelingDS$week2_visibility + 
                                3*ModelingDS$week3_visibility + 
                                4*ModelingDS$week4_visibility +
                                5*ModelingDS$week5_visibility +
                                6*ModelingDS$week6_visibility +
                                7*ModelingDS$week7_visibility +
                                8*ModelingDS$week8_visibility +
                                9*ModelingDS$week9_visibility +
                                10*ModelingDS$week10_visibility)/55
ModelingDS$week10_mavg_vis = (ModelingDS$week8_visibility + 
                                ModelingDS$week9_visibility + 
                                ModelingDS$week10_visibility)/3
ModelingDS$week10_tone_p = ifelse(ModelingDS$week10_tone == "P",1,0) + ModelingDS$week9_tone_p
ModelingDS$week10_tone_n = ifelse(ModelingDS$week10_tone == "N",1,0) + ModelingDS$week9_tone_n
ModelingDS$week10_tone_m = ifelse(ModelingDS$week10_tone == "M",1,0) + ModelingDS$week9_tone_m
ModelingDS$week10_tone_any = ModelingDS$week10_tone_p + ModelingDS$week10_tone_n + ModelingDS$week10_tone_m
ModelingDS$week10_tone_p_chg = ModelingDS$week10_tone_p - ModelingDS$week7_tone_p
ModelingDS$week10_tone_n_chg = ModelingDS$week10_tone_n - ModelingDS$week7_tone_n
ModelingDS$week10_tone_m_chg = ModelingDS$week10_tone_m - ModelingDS$week7_tone_m
ModelingDS$week10_tone_any_chg = ModelingDS$week10_tone_any - ModelingDS$week7_tone_any
ModelingDS$week10_rating_INV = ifelse(ModelingDS$week10_rating == "INV",1,0) + ModelingDS$week9_rating_INV
ModelingDS$week10_rating_UTR = ifelse(ModelingDS$week10_rating == "UTR",1,0) + ModelingDS$week9_rating_UTR
ModelingDS$week10_rating_MOR = ifelse(ModelingDS$week10_rating == "MOR",1,0) + ModelingDS$week9_rating_MOR
ModelingDS$week10_rating_CP = ifelse(ModelingDS$week10_rating == "CP",1,0) + ModelingDS$week9_rating_CP
ModelingDS$week10_rating_OTT = ifelse(ModelingDS$week10_rating == "OTT",1,0) + ModelingDS$week9_rating_OT
ModelingDS$Week10_rating_INV_chg = ModelingDS$week10_rating_INV - ModelingDS$week7_rating_INV
ModelingDS$Week10_rating_UTR_chg = ModelingDS$week10_rating_UTR - ModelingDS$week7_rating_UTR
ModelingDS$Week10_rating_MOR_chg = ModelingDS$week10_rating_MOR - ModelingDS$week7_rating_MOR
ModelingDS$Week10_rating_CP_chg = ModelingDS$week10_rating_CP - ModelingDS$week7_rating_CP
ModelingDS$Week10_rating_OTT_chg = ModelingDS$week10_rating_OTT - ModelingDS$week7_rating_OTT
#Week 11
ModelingDS$week11_avg_vis = (ModelingDS$week1_visibility + 
                               ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility +
                               ModelingDS$week5_visibility +
                               ModelingDS$week6_visibility +
                               ModelingDS$week7_visibility +
                               ModelingDS$week8_visibility +
                               ModelingDS$week9_visibility +
                               ModelingDS$week10_visibility +
                               ModelingDS$week11_visibility)/11
ModelingDS$week11_wavg_vis = (ModelingDS$week1_visibility + 
                                2*ModelingDS$week2_visibility + 
                                3*ModelingDS$week3_visibility + 
                                4*ModelingDS$week4_visibility +
                                5*ModelingDS$week5_visibility +
                                6*ModelingDS$week6_visibility +
                                7*ModelingDS$week7_visibility +
                                8*ModelingDS$week8_visibility +
                                9*ModelingDS$week9_visibility +
                                10*ModelingDS$week10_visibility +
                                11*ModelingDS$week11_visibility)/66
ModelingDS$week11_mavg_vis = (ModelingDS$week9_visibility + 
                                ModelingDS$week10_visibility + 
                                ModelingDS$week11_visibility)/3
ModelingDS$week11_tone_p = ifelse(ModelingDS$week11_tone == "P",1,0) + ModelingDS$week10_tone_p
ModelingDS$week11_tone_n = ifelse(ModelingDS$week11_tone == "N",1,0) + ModelingDS$week10_tone_n
ModelingDS$week11_tone_m = ifelse(ModelingDS$week11_tone == "M",1,0) + ModelingDS$week10_tone_m
ModelingDS$week11_tone_any = ModelingDS$week11_tone_p + ModelingDS$week11_tone_n + ModelingDS$week11_tone_m
ModelingDS$week11_tone_p_chg = ModelingDS$week11_tone_p - ModelingDS$week8_tone_p
ModelingDS$week11_tone_n_chg = ModelingDS$week11_tone_n - ModelingDS$week8_tone_n
ModelingDS$week11_tone_m_chg = ModelingDS$week11_tone_m - ModelingDS$week8_tone_m
ModelingDS$week11_tone_any_chg = ModelingDS$week11_tone_any - ModelingDS$week8_tone_any
ModelingDS$week11_rating_INV = ifelse(ModelingDS$week11_rating == "INV",1,0) + ModelingDS$week10_rating_INV
ModelingDS$week11_rating_UTR = ifelse(ModelingDS$week11_rating == "UTR",1,0) + ModelingDS$week10_rating_UTR
ModelingDS$week11_rating_MOR = ifelse(ModelingDS$week11_rating == "MOR",1,0) + ModelingDS$week10_rating_MOR
ModelingDS$week11_rating_CP = ifelse(ModelingDS$week11_rating == "CP",1,0) + ModelingDS$week10_rating_CP
ModelingDS$week11_rating_OTT = ifelse(ModelingDS$week11_rating == "OTT",1,0) + ModelingDS$week10_rating_OT
ModelingDS$Week11_rating_INV_chg = ModelingDS$week11_rating_INV - ModelingDS$week8_rating_INV
ModelingDS$Week11_rating_UTR_chg = ModelingDS$week11_rating_UTR - ModelingDS$week8_rating_UTR
ModelingDS$Week11_rating_MOR_chg = ModelingDS$week11_rating_MOR - ModelingDS$week8_rating_MOR
ModelingDS$Week11_rating_CP_chg = ModelingDS$week11_rating_CP - ModelingDS$week8_rating_CP
ModelingDS$Week11_rating_OTT_chg = ModelingDS$week11_rating_OTT - ModelingDS$week8_rating_OTT
#Week 12
ModelingDS$week12_avg_vis = (ModelingDS$week1_visibility + 
                               ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility +
                               ModelingDS$week5_visibility +
                               ModelingDS$week6_visibility +
                               ModelingDS$week7_visibility +
                               ModelingDS$week8_visibility +
                               ModelingDS$week9_visibility +
                               ModelingDS$week10_visibility +
                               ModelingDS$week11_visibility +
                               ModelingDS$week12_visibility)/12
ModelingDS$week12_wavg_vis = (ModelingDS$week1_visibility + 
                                2*ModelingDS$week2_visibility + 
                                3*ModelingDS$week3_visibility + 
                                4*ModelingDS$week4_visibility +
                                5*ModelingDS$week5_visibility +
                                6*ModelingDS$week6_visibility +
                                7*ModelingDS$week7_visibility +
                                8*ModelingDS$week8_visibility +
                                9*ModelingDS$week9_visibility +
                                10*ModelingDS$week10_visibility +
                                11*ModelingDS$week11_visibility +
                                12*ModelingDS$week12_visibility)/78
ModelingDS$week12_mavg_vis = (ModelingDS$week10_visibility + 
                                ModelingDS$week11_visibility + 
                                ModelingDS$week12_visibility)/3
ModelingDS$week12_tone_p = ifelse(ModelingDS$week12_tone == "P",1,0) + ModelingDS$week11_tone_p
ModelingDS$week12_tone_n = ifelse(ModelingDS$week12_tone == "N",1,0) + ModelingDS$week11_tone_n
ModelingDS$week12_tone_m = ifelse(ModelingDS$week12_tone == "M",1,0) + ModelingDS$week11_tone_m
ModelingDS$week12_tone_any = ModelingDS$week12_tone_p + ModelingDS$week12_tone_n + ModelingDS$week12_tone_m
ModelingDS$week12_tone_p_chg = ModelingDS$week12_tone_p - ModelingDS$week9_tone_p
ModelingDS$week12_tone_n_chg = ModelingDS$week12_tone_n - ModelingDS$week9_tone_n
ModelingDS$week12_tone_m_chg = ModelingDS$week12_tone_m - ModelingDS$week9_tone_m
ModelingDS$week12_tone_any_chg = ModelingDS$week12_tone_any - ModelingDS$week9_tone_any
ModelingDS$week12_rating_INV = ifelse(ModelingDS$week12_rating == "INV",1,0) + ModelingDS$week11_rating_INV
ModelingDS$week12_rating_UTR = ifelse(ModelingDS$week12_rating == "UTR",1,0) + ModelingDS$week11_rating_UTR
ModelingDS$week12_rating_MOR = ifelse(ModelingDS$week12_rating == "MOR",1,0) + ModelingDS$week11_rating_MOR
ModelingDS$week12_rating_CP = ifelse(ModelingDS$week12_rating == "CP",1,0) + ModelingDS$week11_rating_CP
ModelingDS$week12_rating_OTT = ifelse(ModelingDS$week12_rating == "OTT",1,0) + ModelingDS$week11_rating_OT
ModelingDS$Week12_rating_INV_chg = ModelingDS$week12_rating_INV - ModelingDS$week9_rating_INV
ModelingDS$Week12_rating_UTR_chg = ModelingDS$week12_rating_UTR - ModelingDS$week9_rating_UTR
ModelingDS$Week12_rating_MOR_chg = ModelingDS$week12_rating_MOR - ModelingDS$week9_rating_MOR
ModelingDS$Week12_rating_CP_chg = ModelingDS$week12_rating_CP - ModelingDS$week9_rating_CP
ModelingDS$Week12_rating_OTT_chg = ModelingDS$week12_rating_OTT - ModelingDS$week9_rating_OTT
#Week 13
ModelingDS$week13_avg_vis = (ModelingDS$week1_visibility + 
                               ModelingDS$week2_visibility + 
                               ModelingDS$week3_visibility + 
                               ModelingDS$week4_visibility +
                               ModelingDS$week5_visibility +
                               ModelingDS$week6_visibility +
                               ModelingDS$week7_visibility +
                               ModelingDS$week8_visibility +
                               ModelingDS$week9_visibility +
                               ModelingDS$week10_visibility +
                               ModelingDS$week11_visibility +
                               ModelingDS$week12_visibility +
                               ModelingDS$week13_visibility)/13
ModelingDS$week13_wavg_vis = (ModelingDS$week1_visibility + 
                                2*ModelingDS$week2_visibility + 
                                3*ModelingDS$week3_visibility + 
                                4*ModelingDS$week4_visibility +
                                5*ModelingDS$week5_visibility +
                                6*ModelingDS$week6_visibility +
                                7*ModelingDS$week7_visibility +
                                8*ModelingDS$week8_visibility +
                                9*ModelingDS$week9_visibility +
                                10*ModelingDS$week10_visibility +
                                11*ModelingDS$week11_visibility +
                                12*ModelingDS$week12_visibility +
                                13*ModelingDS$week13_visibility)/91
ModelingDS$week13_mavg_vis = (ModelingDS$week11_visibility + 
                                ModelingDS$week12_visibility + 
                                ModelingDS$week13_visibility)/3
ModelingDS$week13_tone_p = ifelse(ModelingDS$week13_tone == "P",1,0) + ModelingDS$week12_tone_p
ModelingDS$week13_tone_n = ifelse(ModelingDS$week13_tone == "N",1,0) + ModelingDS$week12_tone_n
ModelingDS$week13_tone_m = ifelse(ModelingDS$week13_tone == "M",1,0) + ModelingDS$week12_tone_m
ModelingDS$week13_tone_any = ModelingDS$week13_tone_p + ModelingDS$week13_tone_n + ModelingDS$week13_tone_m
ModelingDS$week13_tone_p_chg = ModelingDS$week13_tone_p - ModelingDS$week10_tone_p
ModelingDS$week13_tone_n_chg = ModelingDS$week13_tone_n - ModelingDS$week10_tone_n
ModelingDS$week13_tone_m_chg = ModelingDS$week13_tone_m - ModelingDS$week10_tone_m
ModelingDS$week13_tone_any_chg = ModelingDS$week13_tone_any - ModelingDS$week10_tone_any
ModelingDS$week13_rating_INV = ifelse(ModelingDS$week13_rating == "INV",1,0) + ModelingDS$week12_rating_INV
ModelingDS$week13_rating_UTR = ifelse(ModelingDS$week13_rating == "UTR",1,0) + ModelingDS$week12_rating_UTR
ModelingDS$week13_rating_MOR = ifelse(ModelingDS$week13_rating == "MOR",1,0) + ModelingDS$week12_rating_MOR
ModelingDS$week13_rating_CP = ifelse(ModelingDS$week13_rating == "CP",1,0) + ModelingDS$week12_rating_CP
ModelingDS$week13_rating_OTT = ifelse(ModelingDS$week13_rating == "OTT",1,0) + ModelingDS$week12_rating_OT
ModelingDS$Week13_rating_INV_chg = ModelingDS$week13_rating_INV - ModelingDS$week10_rating_INV
ModelingDS$Week13_rating_UTR_chg = ModelingDS$week13_rating_UTR - ModelingDS$week10_rating_UTR
ModelingDS$Week13_rating_MOR_chg = ModelingDS$week13_rating_MOR - ModelingDS$week10_rating_MOR
ModelingDS$Week13_rating_CP_chg = ModelingDS$week13_rating_CP - ModelingDS$week10_rating_CP
ModelingDS$Week13_rating_OTT_chg = ModelingDS$week13_rating_OTT - ModelingDS$week10_rating_OTT
for(i in seq(1:dim(ModelingDS)[1])){
  if(is.na(ModelingDS[i,"week2_rating"]) == F){
    if(as.character(ModelingDS[i,"week2_rating"]) == "E") {
      ModelingDS[i,"week2_avg_vis"] = -1
      ModelingDS[i,"week2_wavg_vis"] = -1
      ModelingDS[i,"week2_tone_p"] = -1
      ModelingDS[i,"week2_tone_n"] = -1
      ModelingDS[i,"week2_tone_m"] = -1
      ModelingDS[i,"week2_tone_any"] = -1
      ModelingDS[i,"week2_rating_INV"] = -1
      ModelingDS[i,"week2_rating_UTR"] = -1
      ModelingDS[i,"week2_rating_MOR"] = -1
      ModelingDS[i,"week2_rating_CP"] = -1
      ModelingDS[i,"week2_rating_OTT"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week3_rating"]) == F){
    if(ModelingDS[i,"week3_rating"] == "E"){
      ModelingDS[i,"week3_avg_vis"] = -1
      ModelingDS[i,"week3_wavg_vis"] = -1
      ModelingDS[i,"week3_mavg_vis"] = -1
      ModelingDS[i,"week3_tone_p"] = -1
      ModelingDS[i,"week3_tone_n"] = -1
      ModelingDS[i,"week3_tone_m"] = -1
      ModelingDS[i,"week3_tone_any"] = -1
      ModelingDS[i,"week3_tone_p_chg"] = -1
      ModelingDS[i,"week3_tone_n_chg"] = -1
      ModelingDS[i,"week3_tone_m_chg"] = -1
      ModelingDS[i,"week3_tone_any_chg"] = -1
      ModelingDS[i,"week3_rating_INV"] = -1
      ModelingDS[i,"week3_rating_UTR"] = -1
      ModelingDS[i,"week3_rating_MOR"] = -1
      ModelingDS[i,"week3_rating_CP"] = -1
      ModelingDS[i,"week3_rating_OTT"] = -1
      ModelingDS[i,"Week3_rating_INV_chg"] = -1
      ModelingDS[i,"Week3_rating_UTR_chg"] = -1
      ModelingDS[i,"Week3_rating_MOR_chg"] = -1
      ModelingDS[i,"Week3_rating_CP_chg"] = -1
      ModelingDS[i,"Week3_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week4_rating"]) == F){
    if(ModelingDS[i,"week4_rating"] == "E"){
      ModelingDS[i,"week4_avg_vis"] = -1
      ModelingDS[i,"week4_wavg_vis"] = -1
      ModelingDS[i,"week4_mavg_vis"] = -1
      ModelingDS[i,"week4_tone_p"] = -1
      ModelingDS[i,"week4_tone_n"] = -1
      ModelingDS[i,"week4_tone_m"] = -1
      ModelingDS[i,"week4_tone_any"] = -1
      ModelingDS[i,"week4_tone_p_chg"] = -1
      ModelingDS[i,"week4_tone_n_chg"] = -1
      ModelingDS[i,"week4_tone_m_chg"] = -1
      ModelingDS[i,"week4_tone_any_chg"] = -1
      ModelingDS[i,"week4_rating_INV"] = -1
      ModelingDS[i,"week4_rating_UTR"] = -1
      ModelingDS[i,"week4_rating_MOR"] = -1
      ModelingDS[i,"week4_rating_CP"] = -1
      ModelingDS[i,"week4_rating_OTT"] = -1
      ModelingDS[i,"Week4_rating_INV_chg"] = -1
      ModelingDS[i,"Week4_rating_UTR_chg"] = -1
      ModelingDS[i,"Week4_rating_MOR_chg"] = -1
      ModelingDS[i,"Week4_rating_CP_chg"] = -1
      ModelingDS[i,"Week4_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week5_rating"]) == F){
    if(ModelingDS[i,"week5_rating"] == "E"){
      ModelingDS[i,"week5_avg_vis"] = -1
      ModelingDS[i,"week5_wavg_vis"] = -1
      ModelingDS[i,"week5_mavg_vis"] = -1
      ModelingDS[i,"week5_tone_p"] = -1
      ModelingDS[i,"week5_tone_n"] = -1
      ModelingDS[i,"week5_tone_m"] = -1
      ModelingDS[i,"week5_tone_any"] = -1
      ModelingDS[i,"week5_tone_p_chg"] = -1
      ModelingDS[i,"week5_tone_n_chg"] = -1
      ModelingDS[i,"week5_tone_m_chg"] = -1
      ModelingDS[i,"week5_tone_any_chg"] = -1
      ModelingDS[i,"week5_rating_INV"] = -1
      ModelingDS[i,"week5_rating_UTR"] = -1
      ModelingDS[i,"week5_rating_MOR"] = -1
      ModelingDS[i,"week5_rating_CP"] = -1
      ModelingDS[i,"week5_rating_OTT"] = -1
      ModelingDS[i,"Week5_rating_INV_chg"] = -1
      ModelingDS[i,"Week5_rating_UTR_chg"] = -1
      ModelingDS[i,"Week5_rating_MOR_chg"] = -1
      ModelingDS[i,"Week5_rating_CP_chg"] = -1
      ModelingDS[i,"Week5_rating_OTT_chg"] = -1
    }
  }
  if(is.na(ModelingDS[i,"week6_rating"]) == F){
    if(ModelingDS[i,"week6_rating"] == "E"){
      ModelingDS[i,"week6_avg_vis"] = -1
      ModelingDS[i,"week6_wavg_vis"] = -1
      ModelingDS[i,"week6_mavg_vis"] = -1
      ModelingDS[i,"week6_tone_p"] = -1
      ModelingDS[i,"week6_tone_n"] = -1
      ModelingDS[i,"week6_tone_m"] = -1
      ModelingDS[i,"week6_tone_any"] = -1
      ModelingDS[i,"week6_tone_p_chg"] = -1
      ModelingDS[i,"week6_tone_n_chg"] = -1
      ModelingDS[i,"week6_tone_m_chg"] = -1
      ModelingDS[i,"week6_tone_any_chg"] = -1
      ModelingDS[i,"week6_rating_INV"] = -1
      ModelingDS[i,"week6_rating_UTR"] = -1
      ModelingDS[i,"week6_rating_MOR"] = -1
      ModelingDS[i,"week6_rating_CP"] = -1
      ModelingDS[i,"week6_rating_OTT"] = -1
      ModelingDS[i,"Week6_rating_INV_chg"] = -1
      ModelingDS[i,"Week6_rating_UTR_chg"] = -1
      ModelingDS[i,"Week6_rating_MOR_chg"] = -1
      ModelingDS[i,"Week6_rating_CP_chg"] = -1
      ModelingDS[i,"Week6_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week7_rating"]) == F){
    if(ModelingDS[i,"week7_rating"] == "E"){
      ModelingDS[i,"week7_avg_vis"] = -1
      ModelingDS[i,"week7_wavg_vis"] = -1
      ModelingDS[i,"week7_mavg_vis"] = -1
      ModelingDS[i,"week7_tone_p"] = -1
      ModelingDS[i,"week7_tone_n"] = -1
      ModelingDS[i,"week7_tone_m"] = -1
      ModelingDS[i,"week7_tone_any"] = -1
      ModelingDS[i,"week7_tone_p_chg"] = -1
      ModelingDS[i,"week7_tone_n_chg"] = -1
      ModelingDS[i,"week7_tone_m_chg"] = -1
      ModelingDS[i,"week7_tone_any_chg"] = -1
      ModelingDS[i,"week7_rating_INV"] = -1
      ModelingDS[i,"week7_rating_UTR"] = -1
      ModelingDS[i,"week7_rating_MOR"] = -1
      ModelingDS[i,"week7_rating_CP"] = -1
      ModelingDS[i,"week7_rating_OTT"] = -1
      ModelingDS[i,"Week7_rating_INV_chg"] = -1
      ModelingDS[i,"Week7_rating_UTR_chg"] = -1
      ModelingDS[i,"Week7_rating_MOR_chg"] = -1
      ModelingDS[i,"Week7_rating_CP_chg"] = -1
      ModelingDS[i,"Week7_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week8_rating"]) == F){
    if(ModelingDS[i,"week8_rating"] == "E"){
      ModelingDS[i,"week8_avg_vis"] = -1
      ModelingDS[i,"week8_wavg_vis"] = -1
      ModelingDS[i,"week8_mavg_vis"] = -1
      ModelingDS[i,"week8_tone_p"] = -1
      ModelingDS[i,"week8_tone_n"] = -1
      ModelingDS[i,"week8_tone_m"] = -1
      ModelingDS[i,"week8_tone_any"] = -1
      ModelingDS[i,"week8_tone_p_chg"] = -1
      ModelingDS[i,"week8_tone_n_chg"] = -1
      ModelingDS[i,"week8_tone_m_chg"] = -1
      ModelingDS[i,"week8_tone_any_chg"] = -1
      ModelingDS[i,"week8_rating_INV"] = -1
      ModelingDS[i,"week8_rating_UTR"] = -1
      ModelingDS[i,"week8_rating_MOR"] = -1
      ModelingDS[i,"week8_rating_CP"] = -1
      ModelingDS[i,"week8_rating_OTT"] = -1
      ModelingDS[i,"Week8_rating_INV_chg"] = -1
      ModelingDS[i,"Week8_rating_UTR_chg"] = -1
      ModelingDS[i,"Week8_rating_MOR_chg"] = -1
      ModelingDS[i,"Week8_rating_CP_chg"] = -1
      ModelingDS[i,"Week8_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week9_rating"]) == F){
    if(ModelingDS[i,"week9_rating"] == "E"){
      ModelingDS[i,"week9_avg_vis"] = -1
      ModelingDS[i,"week9_wavg_vis"] = -1
      ModelingDS[i,"week9_mavg_vis"] = -1
      ModelingDS[i,"week9_tone_p"] = -1
      ModelingDS[i,"week9_tone_n"] = -1
      ModelingDS[i,"week9_tone_m"] = -1
      ModelingDS[i,"week9_tone_any"] = -1
      ModelingDS[i,"week9_tone_p_chg"] = -1
      ModelingDS[i,"week9_tone_n_chg"] = -1
      ModelingDS[i,"week9_tone_m_chg"] = -1
      ModelingDS[i,"week9_tone_any_chg"] = -1
      ModelingDS[i,"week9_rating_INV"] = -1
      ModelingDS[i,"week9_rating_UTR"] = -1
      ModelingDS[i,"week9_rating_MOR"] = -1
      ModelingDS[i,"week9_rating_CP"] = -1
      ModelingDS[i,"week9_rating_OTT"] = -1
      ModelingDS[i,"Week9_rating_INV_chg"] = -1
      ModelingDS[i,"Week9_rating_UTR_chg"] = -1
      ModelingDS[i,"Week9_rating_MOR_chg"] = -1
      ModelingDS[i,"Week9_rating_CP_chg"] = -1
      ModelingDS[i,"Week9_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week10_rating"]) == F){
    if(ModelingDS[i,"week10_rating"] == "E"){
      ModelingDS[i,"week10_avg_vis"] = -1
      ModelingDS[i,"week10_wavg_vis"] = -1
      ModelingDS[i,"week10_mavg_vis"] = -1
      ModelingDS[i,"week10_tone_p"] = -1
      ModelingDS[i,"week10_tone_n"] = -1
      ModelingDS[i,"week10_tone_m"] = -1
      ModelingDS[i,"week10_tone_any"] = -1
      ModelingDS[i,"week10_tone_p_chg"] = -1
      ModelingDS[i,"week10_tone_n_chg"] = -1
      ModelingDS[i,"week10_tone_m_chg"] = -1
      ModelingDS[i,"week10_tone_any_chg"] = -1
      ModelingDS[i,"week10_rating_INV"] = -1
      ModelingDS[i,"week10_rating_UTR"] = -1
      ModelingDS[i,"week10_rating_MOR"] = -1
      ModelingDS[i,"week10_rating_CP"] = -1
      ModelingDS[i,"week10_rating_OTT"] = -1
      ModelingDS[i,"Week10_rating_INV_chg"] = -1
      ModelingDS[i,"Week10_rating_UTR_chg"] = -1
      ModelingDS[i,"Week10_rating_MOR_chg"] = -1
      ModelingDS[i,"Week10_rating_CP_chg"] = -1
      ModelingDS[i,"Week10_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week11_rating"]) == F){
    if(ModelingDS[i,"week11_rating"] == "E"){
      ModelingDS[i,"week11_avg_vis"] = -1
      ModelingDS[i,"week11_wavg_vis"] = -1
      ModelingDS[i,"week11_mavg_vis"] = -1
      ModelingDS[i,"week11_tone_p"] = -1
      ModelingDS[i,"week11_tone_n"] = -1
      ModelingDS[i,"week11_tone_m"] = -1
      ModelingDS[i,"week11_tone_any"] = -1
      ModelingDS[i,"week11_tone_p_chg"] = -1
      ModelingDS[i,"week11_tone_n_chg"] = -1
      ModelingDS[i,"week11_tone_m_chg"] = -1
      ModelingDS[i,"week11_tone_any_chg"] = -1
      ModelingDS[i,"week11_rating_INV"] = -1
      ModelingDS[i,"week11_rating_UTR"] = -1
      ModelingDS[i,"week11_rating_MOR"] = -1
      ModelingDS[i,"week11_rating_CP"] = -1
      ModelingDS[i,"week11_rating_OTT"] = -1
      ModelingDS[i,"Week11_rating_INV_chg"] = -1
      ModelingDS[i,"Week11_rating_UTR_chg"] = -1
      ModelingDS[i,"Week11_rating_MOR_chg"] = -1
      ModelingDS[i,"Week11_rating_CP_chg"] = -1
      ModelingDS[i,"Week11_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week12_rating"]) == F){
    if(ModelingDS[i,"week12_rating"] == "E"){
      ModelingDS[i,"week12_avg_vis"] = -1
      ModelingDS[i,"week12_wavg_vis"] = -1
      ModelingDS[i,"week12_mavg_vis"] = -1
      ModelingDS[i,"week12_tone_p"] = -1
      ModelingDS[i,"week12_tone_n"] = -1
      ModelingDS[i,"week12_tone_m"] = -1
      ModelingDS[i,"week12_tone_any"] = -1
      ModelingDS[i,"week12_tone_p_chg"] = -1
      ModelingDS[i,"week12_tone_n_chg"] = -1
      ModelingDS[i,"week12_tone_m_chg"] = -1
      ModelingDS[i,"week12_tone_any_chg"] = -1
      ModelingDS[i,"week12_rating_INV"] = -1
      ModelingDS[i,"week12_rating_UTR"] = -1
      ModelingDS[i,"week12_rating_MOR"] = -1
      ModelingDS[i,"week12_rating_CP"] = -1
      ModelingDS[i,"week12_rating_OTT"] = -1
      ModelingDS[i,"Week12_rating_INV_chg"] = -1
      ModelingDS[i,"Week12_rating_UTR_chg"] = -1
      ModelingDS[i,"Week12_rating_MOR_chg"] = -1
      ModelingDS[i,"Week12_rating_CP_chg"] = -1
      ModelingDS[i,"Week12_rating_OTT_chg"] = -1
    }
  }
  
  if(is.na(ModelingDS[i,"week13_rating"]) == F){
    if(ModelingDS[i,"week13_rating"] == "E"){
      ModelingDS[i,"week13_avg_vis"] = -1
      ModelingDS[i,"week13_wavg_vis"] = -1
      ModelingDS[i,"week13_mavg_vis"] = -1
      ModelingDS[i,"week13_tone_p"] = -1
      ModelingDS[i,"week13_tone_n"] = -1
      ModelingDS[i,"week13_tone_m"] = -1
      ModelingDS[i,"week13_tone_any"] = -1
      ModelingDS[i,"week13_tone_p_chg"] = -1
      ModelingDS[i,"week13_tone_n_chg"] = -1
      ModelingDS[i,"week13_tone_m_chg"] = -1
      ModelingDS[i,"week13_tone_any_chg"] = -1
      ModelingDS[i,"week13_rating_INV"] = -1
      ModelingDS[i,"week13_rating_UTR"] = -1
      ModelingDS[i,"week13_rating_MOR"] = -1
      ModelingDS[i,"week13_rating_CP"] = -1
      ModelingDS[i,"week13_rating_OTT"] = -1
      ModelingDS[i,"Week13_rating_INV_chg"] = -1
      ModelingDS[i,"Week13_rating_UTR_chg"] = -1
      ModelingDS[i,"Week13_rating_MOR_chg"] = -1
      ModelingDS[i,"Week13_rating_CP_chg"] = -1
      ModelingDS[i,"Week13_rating_OTT_chg"] = -1
    }
  }
}

#Loop through seasons...stack results
for(i in seq(from=12,to=40,by=1)){
  cat("Season",i,sep=" ")
  
  temp = ModelingDS %>% filter(SeasonNumber == i)
  
  #Create week and season parameters
  Weeks = ifelse(is.na(temp[1,"week14_rating"]),13,14)
  Season = i
  
  #Create dataset for modeling
  theDataset = ModelingDS %>% filter(SeasonNumber <= i)
  
  #Build placement model
  options(scipen=999)
  predicts = theDataset %>% 
    filter(SeasonNumber == Season) %>%
    select("Name","Placement","Winner","SeasonNumber")
  predicts_old = theDataset %>% 
    filter(SeasonNumber < Season) %>%
    select("Name","Placement","Winner","SeasonNumber")
  y = "Placement"
  h2o.init(nthreads=-1,max_mem_size='6G')
  for(j in seq(from=2,to=Weeks,by=1)){
    holder = 13 + 3*(j-1)
    holder2 = 79 + 21*(j-3)
    
    if(j == 1){
      temp = theDataset[,c(1:holder)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","SeasonNumber","SeasonName","SeasonType","Winner"))
    } else if(j == 2){
      temp = theDataset[,c(1:holder,59:68)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Winner"))
    } else if(j == 3){
      temp = theDataset[,c(1:holder,59:79)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Winner"))
    } else {
      temp = theDataset[,c(1:holder,59:holder2)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Winner"))
    }
    
    train = temp %>% filter(SeasonNumber < Season)
    test = temp %>% filter(SeasonNumber == Season)
    
    train.h2o = as.h2o(train)
    test.h2o = as.h2o(test)
    
    rf_Winner = h2o.randomForest(x = x,
                                 y = y,
                                 training_frame = train.h2o,
                                 nfolds=10,
                                 min_rows = 5,
                                 col_sample_rate_per_tree = 0.8,
                                 sample_rate = 0.6,
                                 ntrees = 250,
                                 seed = 412)
    results = as.data.frame(h2o.predict(rf_Winner,newdata = test.h2o))
    results_old = as.data.frame(h2o.predict(rf_Winner,newdata = train.h2o))
    colnames(results) = paste('week',j,'_place',sep='')
    colnames(results_old) = paste('week',j,'_place',sep='')
    predicts = cbind(predicts,results[1])
    predicts_old = cbind(predicts_old,results_old[1])
  }
  
  #Build Win % Model
  options(scipen=999)
  predicts2 = theDataset %>% 
    filter(SeasonNumber == Season) %>%
    select("Name","Placement","SeasonNumber")
  predicts2_old = theDataset %>% 
    filter(SeasonNumber < Season) %>%
    select("Name","Placement","SeasonNumber")
  y = "Winner"
  h2o.init(nthreads=-1,max_mem_size='6G')
  for(j in seq(from=2,to=Weeks,by=1)){
    holder = 13 + 3*(j-1)
    holder2 = 79 + 21*(j-3)
    
    if(j == 1){
      temp = theDataset[,c(1:holder)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","SeasonNumber","SeasonName","SeasonType","Placement"))
    } else if(j == 2){
      temp = theDataset[,c(1:holder,59:68)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Placement"))
    } else if(j == 3){
      temp = theDataset[,c(1:holder,59:79)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Placement"))
    } else {
      temp = theDataset[,c(1:holder,59:holder2)]
      x = setdiff(names(temp),c(y,"FirstName","LastName","Nickname","Name","Archetype","SeasonNumber","SeasonName","SeasonType","Placement"))
    }
    
    train = temp %>% filter(SeasonNumber < Season)
    test = temp %>% filter(SeasonNumber == Season)
    
    train.h2o = as.h2o(train)
    test.h2o = as.h2o(test)
    
    rf_Winner = h2o.randomForest(x = x,
                                 y = y,
                                 training_frame = train.h2o,
                                 nfolds=10,
                                 ntrees = 250,
                                 col_sample_rate_per_tree = 0.9,
                                 min_rows = 5,
                                 sample_rate = 0.9,
                                 seed = 412)
    results = as.data.frame(h2o.predict(rf_Winner,newdata = test.h2o))
    results_old = as.data.frame(h2o.predict(rf_Winner,newdata = train.h2o))
    colnames(results) = paste('week',j,'_win',sep='')
    colnames(results_old) = paste('week',j,'_win',sep='')
    predicts2 = cbind(predicts2,results[1])
    predicts2_old = cbind(predicts2_old,results_old[1])
  }
  
  #create null if no week 14
  if(!"week14_place" %in% colnames(predicts)){
    predicts = predicts %>% mutate(week14_place = NA)
    predicts2 = predicts2 %>% mutate(week14_win = NA)
    
    predicts_old = predicts_old %>% mutate(week14_place = NA)
    predicts2_old = predicts2_old %>% mutate(week14_win = NA)
  }
  
  #join data
  model_results = predicts %>% inner_join(.,predicts2,by=c("Name","Placement","SeasonNumber")) %>% mutate(SeasonModel = i)
  model_results_old = predicts_old %>% inner_join(.,predicts2_old,by=c("Name","Placement","SeasonNumber")) %>% mutate(SeasonModel = i)

  #stack results
  if(i==12){
    final_predicts = model_results
    final_predicts_old = model_results_old
  } else {
    final_predicts = rbind(final_predicts,model_results)
    final_predicts_old = rbind(final_predicts_old,model_results_old)
  }
  
  #clear all h2o elements
  h2o.removeAll()
}

write.csv(final_predicts,"survivor_model_results.csv",row.names=FALSE)
write.csv(final_predicts_old,"survivor_model_train_results.csv",row.names=FALSE)


# ------------------------------   EVALUATE RESULTS   ------------------------------

#Read datasets
survivor_model = read.csv("survivor_model_results.csv",header = T,stringsAsFactors = F)
survivor_model_hist = read.csv("survivor_model_train_results.csv",header = T,stringsAsFactors = F)

#Build Elimination Week on ModelingDS
ModelingDS$EliminationWeek = NA
for(i in seq(1:dim(ModelingDS)[1])){
  if(is.na(ModelingDS[i,"week2_rating"])==F) {
    if(ModelingDS[i,"week2_rating"] == "E"){
      ModelingDS[i,"EliminationWeek"] = 1
    }
  }
  
  if(is.na(ModelingDS[i,"week3_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week3_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 2
    }
  } 
  
  if(is.na(ModelingDS[i,"week4_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week4_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 3
    }
  } 
  
  if(is.na(ModelingDS[i,"week5_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week5_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 4
    }
  } 
  
  if(is.na(ModelingDS[i,"week6_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week6_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 5
    }
  } 
  
  if(is.na(ModelingDS[i,"week7_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week7_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 6
    }
  } 
  
  if(is.na(ModelingDS[i,"week8_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week8_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 7
    }
  } 
  
  if(is.na(ModelingDS[i,"week9_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week9_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 8
    }
  } 
  
  if(is.na(ModelingDS[i,"week10_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week10_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 9
    }
  } 
  
  if(is.na(ModelingDS[i,"week11_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week11_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 10
    }
  } 
  
  if(is.na(ModelingDS[i,"week12_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week12_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 11
    }
  } 
  
  if(is.na(ModelingDS[i,"week13_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week13_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 12
    }
  } 
  
  if(is.na(ModelingDS[i,"week14_rating"])==F  && is.na(ModelingDS[i,"EliminationWeek"])==T) {
    if(ModelingDS[i,"week14_rating"]=="E"){
      ModelingDS[i,"EliminationWeek"] = 13
    }
  } 
}

#Loop through results
for(j in seq(from=min(survivor_model$SeasonNumber),to=max(survivor_model$SeasonNumber),by=1)){
  temp = survivor_model %>% filter(SeasonNumber==j)
  
  #Build elimination week logic
  if(is.na(temp[1,"week14_place"])==T){
    SeasonWeeks = 13
  } else {
    SeasonWeeks = 14
  }
  
  current = ModelingDS %>% filter(SeasonNumber == j)
  current$EliminationWeek = NA
  for(i in seq(1:dim(current)[1])){
    if(is.na(current[i,"week2_rating"])==F) {
      if(current[i,"week2_rating"] == "E"){
        current[i,"EliminationWeek"] = 1
      }
    }
    
    if(is.na(current[i,"week3_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week3_rating"]=="E"){
        current[i,"EliminationWeek"] = 2
      }
    } 
    
    if(is.na(current[i,"week4_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week4_rating"]=="E"){
        current[i,"EliminationWeek"] = 3
      }
    } 
    
    if(is.na(current[i,"week5_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week5_rating"]=="E"){
        current[i,"EliminationWeek"] = 4
      }
    } 
    
    if(is.na(current[i,"week6_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week6_rating"]=="E"){
        current[i,"EliminationWeek"] = 5
      }
    } 
    
    if(is.na(current[i,"week7_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week7_rating"]=="E"){
        current[i,"EliminationWeek"] = 6
      }
    } 
    
    if(is.na(current[i,"week8_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week8_rating"]=="E"){
        current[i,"EliminationWeek"] = 7
      }
    } 
    
    if(is.na(current[i,"week9_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week9_rating"]=="E"){
        current[i,"EliminationWeek"] = 8
      }
    } 
    
    if(is.na(current[i,"week10_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week10_rating"]=="E"){
        current[i,"EliminationWeek"] = 9
      }
    } 
    
    if(is.na(current[i,"week11_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week11_rating"]=="E"){
        current[i,"EliminationWeek"] = 10
      }
    } 
    
    if(is.na(current[i,"week12_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week12_rating"]=="E"){
        current[i,"EliminationWeek"] = 11
      }
    } 
    
    if(is.na(current[i,"week13_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week13_rating"]=="E"){
        current[i,"EliminationWeek"] = 12
      }
    } 
    
    if(is.na(current[i,"week14_rating"])==F  && is.na(current[i,"EliminationWeek"])==T) {
      if(current[i,"week14_rating"]=="E"){
        current[i,"EliminationWeek"] = 13
      }
    } 
  }
  
  current = current %>% select("Name","EliminationWeek")
  temp = temp %>% inner_join(.,current,by="Name")
  
  for(i in seq(from=1,to=dim(temp)[1],by=1)){
    if(!is.na(temp[i,"EliminationWeek"])){
      for(k in seq(from=temp[i,"EliminationWeek"]+1,to=SeasonWeeks,by=1)){
        temp[i,paste("week",k,"_place",sep='')] = temp[i,"Placement"]
        temp[i,paste("week",k,"_win",sep='')] = 0
      }
    }
  }
  
  #create adjusted dataset based on known information
  if(j==12){
    survivor_model_adj = temp
  } else {
    survivor_model_adj = rbind(survivor_model_adj,temp)
  }
}

for(i in seq(from=20,to=max(survivor_model_adj$SeasonNumber),by=1)){
  cat("Season: ",i,"\n",sep="")
  
  theTrain = survivor_model_adj %>% filter(SeasonNumber < i)
  theTest = survivor_model_adj %>% filter(SeasonNumber == i)
  
  #Logistic Regression - Kitchen Sink
  week2_ks = glm(Winner ~ week2_place + week2_win, 
                 data=theTrain, family="binomial")
  week3_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win, 
                 data=theTrain, family="binomial")
  week4_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win, 
                 data=theTrain, family="binomial")
  week5_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                   week5_place + week5_win, 
                 data=theTrain, family="binomial")
  week6_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                   week5_place + week5_win + week6_place + week6_win, 
                 data=theTrain, family="binomial")
  week7_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                   week5_place + week5_win + week6_place + week6_win + week7_place + week7_win, 
                 data=theTrain, family="binomial")
  week8_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                   week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                   week8_place + week8_win, 
                 data=theTrain, family="binomial")
  week9_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                   week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                   week8_place + week8_win + week9_place + week9_win, 
                 data=theTrain, family="binomial")
  week10_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win, 
                  data=theTrain, family="binomial")
  week11_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win +
                    week11_place + week11_win, 
                  data=theTrain, family="binomial")
  week12_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win +
                    week11_place + week11_win + week12_place + week12_win, 
                  data=theTrain, family="binomial")
  week13_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win +
                    week11_place + week11_win + week12_place + week12_win + week13_place + week13_win, 
                  data=theTrain, family="binomial")
  week14_ks = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win +
                    week11_place + week11_win + week12_place + week12_win + week13_place + week13_win +
                    week14_place + week14_win, 
                  data=theTrain %>% filter(!is.na(week14_place)), family="binomial")
  
  theTest$week2_pred_logks = predict(week2_ks,theTest,type="response")
  theTest$week3_pred_logks = predict(week3_ks,theTest,type="response")
  theTest$week4_pred_logks = predict(week4_ks,theTest,type="response")
  theTest$week5_pred_logks = predict(week5_ks,theTest,type="response")
  theTest$week6_pred_logks = predict(week6_ks,theTest,type="response")
  theTest$week7_pred_logks = predict(week7_ks,theTest,type="response")
  theTest$week8_pred_logks = predict(week8_ks,theTest,type="response")
  theTest$week9_pred_logks = predict(week9_ks,theTest,type="response")
  theTest$week10_pred_logks = predict(week10_ks,theTest,type="response")
  theTest$week11_pred_logks = predict(week11_ks,theTest,type="response")
  theTest$week12_pred_logks = predict(week12_ks,theTest,type="response")
  theTest$week13_pred_logks = predict(week13_ks,theTest,type="response")
  theTest$week14_pred_logks = predict(week14_ks,theTest,type="response")
  
  #Logistic Regression - Recent
  week2_re = glm(Winner ~ week2_place + week2_win, 
                 data=theTrain, family="binomial")
  week3_re = glm(Winner ~ week2_place + week2_win + week3_place + week3_win, 
                 data=theTrain, family="binomial")
  week4_re = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win, 
                 data=theTrain, family="binomial")
  week5_re = glm(Winner ~ week3_place + week3_win + week4_place + week4_win + week5_place + week5_win, 
                 data=theTrain, family="binomial")
  week6_re = glm(Winner ~ week4_place + week4_win + week5_place + week5_win + week6_place + week6_win, 
                 data=theTrain, family="binomial")
  week7_re = glm(Winner ~ week5_place + week5_win + week6_place + week6_win + week7_place + week7_win, 
                 data=theTrain, family="binomial")
  week8_re = glm(Winner ~ week6_place + week6_win + week7_place + week7_win + week8_place + week8_win, 
                 data=theTrain, family="binomial")
  week9_re = glm(Winner ~ week7_place + week7_win + week8_place + week8_win + week9_place + week9_win, 
                 data=theTrain, family="binomial")
  week10_re = glm(Winner ~ week8_place + week8_win + week9_place + week9_win + week10_place + week10_win, 
                  data=theTrain, family="binomial")
  week11_re = glm(Winner ~ week9_place + week9_win + week10_place + week10_win + week11_place + week11_win, 
                  data=theTrain, family="binomial")
  week12_re = glm(Winner ~ week2_place + week2_win + week3_place + week3_win + week4_place + week4_win +
                    week5_place + week5_win + week6_place + week6_win + week7_place + week7_win +
                    week8_place + week8_win + week9_place + week9_win + week10_place + week10_win +
                    week11_place + week11_win + week12_place + week12_win, 
                  data=theTrain, family="binomial")
  week13_re = glm(Winner ~ week11_win + week12_place + week12_win + week13_place + week13_win, 
                  data=theTrain, family="binomial")
  week14_re = glm(Winner ~ week12_place + week12_win + week13_place + week13_win + week14_place + week14_win, 
                  data=theTrain %>% filter(!is.na(week14_place)), family="binomial")
  
  theTest$week2_pred_logre = predict(week2_re,theTest,type="response")
  theTest$week3_pred_logre = predict(week3_re,theTest,type="response")
  theTest$week4_pred_logre = predict(week4_re,theTest,type="response")
  theTest$week5_pred_logre = predict(week5_re,theTest,type="response")
  theTest$week6_pred_logre = predict(week6_re,theTest,type="response")
  theTest$week7_pred_logre = predict(week7_re,theTest,type="response")
  theTest$week8_pred_logre = predict(week8_re,theTest,type="response")
  theTest$week9_pred_logre = predict(week9_re,theTest,type="response")
  theTest$week10_pred_logre = predict(week10_re,theTest,type="response")
  theTest$week11_pred_logre = predict(week11_re,theTest,type="response")
  theTest$week12_pred_logre = predict(week12_re,theTest,type="response")
  theTest$week13_pred_logre = predict(week13_re,theTest,type="response")
  theTest$week14_pred_logre = predict(week14_re,theTest,type="response")
  
  #Logistic - Bidirectional Stepwise
  Null_Model = glm(Winner ~ 1, data=theTrain, family="binomial")
  Null_Model14 = glm(Winner ~ 1, data=theTrain %>% filter(!is.na(week14_place)), family="binomial")
  
  week2_bi = step(Null_Model,scope=list(upper=week2_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week3_bi = step(Null_Model,scope=list(upper=week3_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week4_bi = step(Null_Model,scope=list(upper=week4_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week5_bi = step(Null_Model,scope=list(upper=week5_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week6_bi = step(Null_Model,scope=list(upper=week6_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week7_bi = step(Null_Model,scope=list(upper=week7_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week8_bi = step(Null_Model,scope=list(upper=week8_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week9_bi = step(Null_Model,scope=list(upper=week9_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week10_bi = step(Null_Model,scope=list(upper=week10_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week11_bi = step(Null_Model,scope=list(upper=week11_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week12_bi = step(Null_Model,scope=list(upper=week12_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week13_bi = step(Null_Model,scope=list(upper=week13_ks),data=theTrain, family="binomial",direction="both",trace=0)
  week14_bi = step(Null_Model14,scope=list(upper=week14_ks),data=theTrain %>% filter(!is.na(week14_place)), family="binomial",direction="both",trace=0)
  
  theTest$week2_pred_logbi = predict(week2_bi,theTest,type="response")
  theTest$week3_pred_logbi = predict(week3_bi,theTest,type="response")
  theTest$week4_pred_logbi = predict(week4_bi,theTest,type="response")
  theTest$week5_pred_logbi = predict(week5_bi,theTest,type="response")
  theTest$week6_pred_logbi = predict(week6_bi,theTest,type="response")
  theTest$week7_pred_logbi = predict(week7_bi,theTest,type="response")
  theTest$week8_pred_logbi = predict(week8_bi,theTest,type="response")
  theTest$week9_pred_logbi = predict(week9_bi,theTest,type="response")
  theTest$week10_pred_logbi = predict(week10_bi,theTest,type="response")
  theTest$week11_pred_logbi = predict(week11_bi,theTest,type="response")
  theTest$week12_pred_logbi = predict(week12_bi,theTest,type="response")
  theTest$week13_pred_logbi = predict(week13_bi,theTest,type="response")
  theTest$week14_pred_logbi = predict(week14_bi,theTest,type="response")
  
  #Random Forest
  h2o.init()
  train_survivor_adj = as.h2o(theTrain)
  train_survivor_adj14 = as.h2o(theTrain %>% filter(!is.na(week14_place)))
  test_survivor_adj = as.h2o(theTest)
  y = "Winner"
  
  x = c("week2_place","week2_win")
  rf_prob2 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win")
  rf_prob3 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win")
  rf_prob4 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win")
  rf_prob5 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win")
  rf_prob6 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win")
  rf_prob7 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win")
  rf_prob8 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win")
  rf_prob9 = h2o.randomForest(x = x,
                              y = y,
                              training_frame = train_survivor_adj,
                              nfolds=10,
                              min_rows = 5,
                              col_sample_rate_per_tree = 0.8,
                              sample_rate = 0.6,
                              ntrees = 250,
                              seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win",
        "week10_place","week10_win")
  rf_prob10 = h2o.randomForest(x = x,
                               y = y,
                               training_frame = train_survivor_adj,
                               nfolds=10,
                               min_rows = 5,
                               col_sample_rate_per_tree = 0.8,
                               sample_rate = 0.6,
                               ntrees = 250,
                               seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win",
        "week10_place","week10_win","week11_place","week11_win")
  rf_prob11 = h2o.randomForest(x = x,
                               y = y,
                               training_frame = train_survivor_adj,
                               nfolds=10,
                               min_rows = 5,
                               col_sample_rate_per_tree = 0.8,
                               sample_rate = 0.6,
                               ntrees = 250,
                               seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win",
        "week10_place","week10_win","week11_place","week11_win","week12_place","week12_win")
  rf_prob12 = h2o.randomForest(x = x,
                               y = y,
                               training_frame = train_survivor_adj,
                               nfolds=10,
                               min_rows = 5,
                               col_sample_rate_per_tree = 0.8,
                               sample_rate = 0.6,
                               ntrees = 250,
                               seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win",
        "week10_place","week10_win","week11_place","week11_win","week12_place","week12_win","week13_place","week13_win")
  rf_prob13 = h2o.randomForest(x = x,
                               y = y,
                               training_frame = train_survivor_adj,
                               nfolds=10,
                               min_rows = 5,
                               col_sample_rate_per_tree = 0.8,
                               sample_rate = 0.6,
                               ntrees = 250,
                               seed = 412)
  
  x = c("week2_place","week2_win","week3_place","week3_win","week4_place","week4_win","week5_place","week5_win",
        "week6_place","week6_win","week7_place","week7_win","week8_place","week8_win","week9_place","week9_win",
        "week10_place","week10_win","week11_place","week11_win","week12_place","week12_win","week13_place","week13_win",
        "week14_place","week14_win")
  rf_prob14 = h2o.randomForest(x = x,
                               y = y,
                               training_frame = train_survivor_adj14,
                               nfolds=10,
                               min_rows = 5,
                               col_sample_rate_per_tree = 0.8,
                               sample_rate = 0.6,
                               ntrees = 250,
                               seed = 412)
  
  theTest$week2_pred_rf = as.vector(h2o.predict(rf_prob2,test_survivor_adj))
  theTest$week3_pred_rf = as.vector(h2o.predict(rf_prob3,test_survivor_adj))
  theTest$week4_pred_rf = as.vector(h2o.predict(rf_prob4,test_survivor_adj))
  theTest$week5_pred_rf = as.vector(h2o.predict(rf_prob5,test_survivor_adj))
  theTest$week6_pred_rf = as.vector(h2o.predict(rf_prob6,test_survivor_adj))
  theTest$week7_pred_rf = as.vector(h2o.predict(rf_prob7,test_survivor_adj))
  theTest$week8_pred_rf = as.vector(h2o.predict(rf_prob8,test_survivor_adj))
  theTest$week9_pred_rf = as.vector(h2o.predict(rf_prob9,test_survivor_adj))
  theTest$week10_pred_rf = as.vector(h2o.predict(rf_prob10,test_survivor_adj))
  theTest$week11_pred_rf = as.vector(h2o.predict(rf_prob11,test_survivor_adj))
  theTest$week12_pred_rf = as.vector(h2o.predict(rf_prob12,test_survivor_adj))
  theTest$week13_pred_rf = as.vector(h2o.predict(rf_prob13,test_survivor_adj))
  theTest$week14_pred_rf = as.vector(h2o.predict(rf_prob14,test_survivor_adj))
  
  h2o.removeAll()
  
  
  #Get Season Weeks
  if(is.na(theTest[1,"week14_place"])==T){
    SeasonWeeks = 13
  } else {
    SeasonWeeks = 14
  }
  
  #make adjustments based on knowns... 
  for(j in seq(from=1,to=dim(theTest)[1],by=1)){
    if(!is.na(theTest[j,"EliminationWeek"])){
      for(k in seq(from=theTest[j,"EliminationWeek"]+1,to=SeasonWeeks,by=1)){
        theTest[j,paste("week",k,"_win",sep='')] = 0
        theTest[j,paste("week",k,"_pred_logks",sep='')] = 0
        theTest[j,paste("week",k,"_pred_logre",sep='')] = 0
        theTest[j,paste("week",k,"_pred_logbi",sep='')] = 0
        theTest[j,paste("week",k,"_pred_rf",sep='')] = 0
      }
    }
  }
  
  #Readjust after the knowns to scale to 100
  for(k in seq(from=1,to=dim(theTest)[2],by=1)){
    if(str_detect(colnames(theTest)[k],"_win") | str_detect(colnames(theTest)[k],"_pred")){
      theSum = sum(theTest[,k])
      for(j in seq(from=1,to=dim(theTest)[1],by=1)){
        theTest[j,k] = theTest[j,k] / theSum
      }
    }
  }
  
  #Calculate differentials and start building table of stored values
  for(j in seq(from=2,to=SeasonWeeks,by=1)){
    win_abserr = sum(abs(theTest$Winner - theTest[,paste("week",j,"_win",sep='')]))
    win_sse = sum((theTest$Winner - theTest[,paste("week",j,"_win",sep='')])^2)
    logks_abserr = sum(abs(theTest$Winner - theTest[,paste("week",j,"_pred_logks",sep='')]))
    logks_sse = sum((theTest$Winner - theTest[,paste("week",j,"_pred_logks",sep='')])^2)
    logre_abserr = sum(abs(theTest$Winner - theTest[,paste("week",j,"_pred_logre",sep='')]))
    logre_sse = sum((theTest$Winner - theTest[,paste("week",j,"_pred_logre",sep='')])^2)
    logbi_abserr = sum(abs(theTest$Winner - theTest[,paste("week",j,"_pred_logbi",sep='')]))
    logbi_sse = sum((theTest$Winner - theTest[,paste("week",j,"_pred_logbi",sep='')])^2)
    rf_abserr = sum(abs(theTest$Winner - theTest[,paste("week",j,"_pred_rf",sep='')]))
    rf_sse = sum((theTest$Winner - theTest[,paste("week",j,"_pred_rf",sep='')])^2)
    
    #Create results table
    if(i==20 & j==2){
      results_table = as.data.frame(cbind(i,j,
                                          win_abserr,logks_abserr,logre_abserr,logbi_abserr,rf_abserr,
                                          win_sse,logks_sse,logre_sse,logbi_sse,rf_sse))
    } else {
      results_table = rbind(results_table,
                            as.data.frame(cbind(i,j,
                                                win_abserr,logks_abserr,logre_abserr,logbi_abserr,rf_abserr,
                                                win_sse,logks_sse,logre_sse,logbi_sse,rf_sse)))
    }
  }
  
  #Stack final adjusted datasets
  if(i==20){
    final_adj = theTest
  } else {
    final_adj = rbind(final_adj,theTest)
  }
}

h2o.shutdown()



#Write CSV
write.csv(final_adj,"final_adjusted_results.csv",row.names=F)

colnames(results_table)[1:2] = c("Season","Week")
write.csv(results_table,"model_results_table.csv",row.names=F)

#Build some plots
for(i in seq(from=min(results_table$Week),to=max(results_table$Week),by=1)){
  holder = results_table %>% filter(Week == i)
  
  abs = holder[,c("Season","win_abserr","logks_abserr","logre_abserr","logbi_abserr","rf_abserr")]
  sse = holder[,c("Season","win_sse","logks_sse","logre_sse","logbi_sse","rf_sse")]
  
  abs = abs %>% gather(key="model",value="absolute_error",2:6)
  sse = sse %>% gather(key="model",value="sse",2:6)
  
  
  abs_plot = abs %>%
                ggplot(aes(x=Season,y=absolute_error,color=model)) +
                geom_point() +
                geom_line() +
                labs(y="Absoulte Error",x="Season") +
                ggtitle(paste("Absolute Error: Week",i,sep=" "))
  ggsave(abs_plot,file=paste("ggplot/abs_err/week",i,".png",sep=""),width=14,height=10)
  
  sse_plot = sse %>%
              ggplot(aes(x=Season,y=sse,color=model)) +
              geom_point() +
              geom_line() +
              labs(y="Sum of Squared Error",x="Season") +
              ggtitle(paste("Sum of Squared Error: Week",i,sep=" "))
  ggsave(sse_plot,file=paste("ggplot/sse/week",i,".png",sep=""),width=14,height=10)
}

#Seasons 38 and 40 have unusually high errors... probably due to Edge of Extinction
#No visible improvement over original random forest win model


#See if placement weighting helps lower error in earlier weeks
for(i in seq(from=min(survivor_model_adj$SeasonNumber),to=max(survivor_model_adj$SeasonNumber),by=1)){
  cat(i,"\n",sep="")
  
  temp = survivor_model_adj %>% filter(SeasonNumber == i)
  
  if(is.na(temp[1,"week14_place"])==T){
    SeasonWeeks = 13
  } else {
    SeasonWeeks = 14
  }
  
  for(j in seq(from=2,to=SeasonWeeks,by=1)){
    holder = temp %>% 
                mutate(week = j) %>%
                select(c("Name","Placement","Winner","SeasonNumber","week","EliminationWeek",
                         paste('week',j,'_place',sep=''),paste('week',j,'_win',sep='')))
    colnames(holder)[c(7,8)] = c("place","win") 
    
    #Create place adjustment... which converts placement into win % out of 100
    holder$place_adj = ifelse(is.na(holder[,"EliminationWeek"]),
                              (1-(holder[,"place"]-min(holder[,"place"],na.rm=T))/
                                 (max(holder[,"place"],na.rm=T)-min(holder[,"place"],na.rm=T))),
                              ifelse(holder[,"EliminationWeek"]<holder[,"week"],0,
                                     (1-(holder[,"place"]-min(holder[,"place"],na.rm=T))/
                                        (max(holder[,"place"],na.rm=T)-min(holder[,"place"],na.rm=T)))))

    holder$place_adj = holder$place_adj / sum(holder$place_adj)
    
    #Create win adjustment... which converts win % to % out of 100 after accounting for actuals
    holder$win_adj = holder$win / sum(holder$win)
    
    for(k in seq(from=0,to=1,by=0.05)){
      place_weight = k
      win_weight = 1-k
      
      theCalc = holder %>% mutate(win_final = place_adj * place_weight + win_adj * win_weight,
                                  place_weight = place_weight,
                                  win_weight = win_weight)

      #Calculate miss
      abs_err = sum(abs(theCalc$Winner - theCalc$win_final))
      sse = sum((theCalc$Winner - theCalc$win_final)^2)
      
      #Stack results
      if(i==min(survivor_model_adj$SeasonNumber) & j == 2 & k == 0){
        final_weight_stack = theCalc
        final_weight_stack_miss = as.data.frame(cbind(i,j,place_weight,win_weight,abs_err,sse))
      } else {
        final_weight_stack = rbind(final_weight_stack,theCalc)
        final_weight_stack_miss = rbind(final_weight_stack_miss,
                                        as.data.frame(cbind(i,j,place_weight,win_weight,abs_err,sse)))
      }
    }
  }
}
colnames(final_weight_stack_miss) = c("Season","Week","place_weight","win_weight","abs_err","sse")

#Build plots to view weekly weighting results
for(i in seq(from=min(final_weight_stack_miss$Week),to=max(final_weight_stack_miss$Week),by=1)){
  weighted = final_weight_stack_miss %>% filter(Week == i) %>% filter(!Season %in% c(38,40)) %>% 
    mutate(group = ifelse(Season < 20,1,
                          ifelse(Season < 30, 2, 3)))
  weighted$group = as.factor(weighted$group)
  
  #Create Final Metrics
  weighted2 = weighted %>% 
    group_by(place_weight,win_weight) %>%
    summarise(mean_sse = mean(sse,na.rm=T),
              sd_sse = sd(sse,na.rm=T))
  
  sse_weight_plot1 = weighted2 %>%
                      ggplot(aes(x=place_weight,y=mean_sse)) +
                      geom_point() +
                      labs(y="SSE",x="Placement Weight") +
                      ggtitle(paste("Week",i,sep=" "))
  ggsave(sse_weight_plot1,file=paste("ggplot/weight place win/mean/week",i,".png",sep=""),width=14,height=10)
  
  sse_weight_plot2 = weighted2 %>%
                      ggplot(aes(x=place_weight,y=sd_sse)) +
                      geom_point() +
                      labs(y="SSE",x="Placement Weight") +
                      ggtitle(paste("Week",i,sep=" "))
  ggsave(sse_weight_plot2,file=paste("ggplot/weight place win/sd/week",i,".png",sep=""),width=14,height=10)
  
  #Create Final Metrics by Era... to ensure there have not been editing differences over time
  weighted3 = weighted %>% 
    group_by(place_weight,win_weight,group) %>%
    summarise(mean_sse = mean(sse,na.rm=T),
              sd_sse = sd(sse,na.rm=T))
  
  sse_weight_plot3 = weighted3 %>%
                      ggplot(aes(x=place_weight,y=mean_sse,color=group)) +
                      geom_point() +
                      geom_line() +
                      labs(y="SSE",x="Placement Weight") +
                      ggtitle(paste("Week",i,sep=" "))
  ggsave(sse_weight_plot3,file=paste("ggplot/weight place win era/mean/week",i,".png",sep=""),width=14,height=10)
  
  sse_weight_plot4 = weighted3 %>%
                      ggplot(aes(x=place_weight,y=sd_sse,color=group)) +
                      geom_point() +
                      geom_line() +
                      labs(y="SSE",x="Placement Weight") +
                      ggtitle(paste("Week",i,sep=" "))
  ggsave(sse_weight_plot4,file=paste("ggplot/weight place win era/sd/week",i,".png",sep=""),width=14,height=10)
}



