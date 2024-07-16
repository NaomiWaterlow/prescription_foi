# code not used


all_ICB_data[is.na(UNIQUE_PATIENT_COUNT), PATIENTS_CAT :=  "1-4"]
all_ICB_data[UNIQUE_PATIENT_COUNT == 0, PATIENTS_CAT :=  "0"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 5 & UNIQUE_PATIENT_COUNT < 10,
             PATIENTS_CAT :=  "5-9"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 10 & UNIQUE_PATIENT_COUNT < 15,
             PATIENTS_CAT :=  "10-14"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 15 & UNIQUE_PATIENT_COUNT < 20,
             PATIENTS_CAT :=  "15-19"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 20 & UNIQUE_PATIENT_COUNT < 25,
             PATIENTS_CAT :=  "20-24"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 25 & UNIQUE_PATIENT_COUNT < 30,
             PATIENTS_CAT :=  "25-29"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 30 & UNIQUE_PATIENT_COUNT < 35,
             PATIENTS_CAT :=  "30-34"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 35 & UNIQUE_PATIENT_COUNT < 40,
             PATIENTS_CAT :=  "35-39"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 40 & UNIQUE_PATIENT_COUNT < 45,
             PATIENTS_CAT :=  "40-44"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 45 & UNIQUE_PATIENT_COUNT < 50,
             PATIENTS_CAT :=  "45-49"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 50 & UNIQUE_PATIENT_COUNT < 55,
             PATIENTS_CAT :=  "50-54"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 55 & UNIQUE_PATIENT_COUNT < 60,
             PATIENTS_CAT :=  "55-59"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 60 & UNIQUE_PATIENT_COUNT < 65,
             PATIENTS_CAT :=  "60-64"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 65 & UNIQUE_PATIENT_COUNT < 70,
             PATIENTS_CAT :=  "65-69"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 70 & UNIQUE_PATIENT_COUNT < 75,
             PATIENTS_CAT :=  "70-74"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 75 & UNIQUE_PATIENT_COUNT < 80,
             PATIENTS_CAT :=  "75-79"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 80 & UNIQUE_PATIENT_COUNT < 85,
             PATIENTS_CAT :=  "80-84"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 85 & UNIQUE_PATIENT_COUNT < 90,
             PATIENTS_CAT :=  "85-89"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 90 & UNIQUE_PATIENT_COUNT < 95,
             PATIENTS_CAT :=  "90-94"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 95 & UNIQUE_PATIENT_COUNT < 100,
             PATIENTS_CAT :=  "95-100"]
all_ICB_data[UNIQUE_PATIENT_COUNT >= 100, PATIENTS_CAT :=  "100+"]

# convert to ordered factor
all_ICB_data[,PATIENTS_CAT := factor(PATIENTS_CAT, levels = 
                                       c("0", "1-4", "5-9", "10-14", 
                                         "15-19", "20-24", "25-29", 
                                         "30-34", "35-39", "40-44",
                                         "45-49", "50-54", "55-59",  
                                         "60-64", "65-69", "70-74",
                                         "75-79", "80-84", "85-89", 
                                         "90-94", "95-100", "100+"))]



# Convert Items to categorical
# convert to numeric -> the "*"s get converted to NAs
all_ICB_data[, ITEMS := as.numeric(ITEMS)]
all_ICB_data[is.na(ITEMS), ITEMS_CAT :=  "1-4"]
all_ICB_data[ITEMS == 0, ITEMS_CAT :=  "0"]
all_ICB_data[ITEMS >= 5 & ITEMS < 10,
             ITEMS_CAT :=  "5-9"]
all_ICB_data[ITEMS >= 10 & ITEMS < 15,
             ITEMS_CAT :=  "10-14"]
all_ICB_data[ITEMS >= 15 & ITEMS < 20,
             ITEMS_CAT :=  "15-19"]
all_ICB_data[ITEMS >= 20 & ITEMS < 25,
             ITEMS_CAT :=  "20-24"]
all_ICB_data[ITEMS >= 25 & ITEMS < 30,
             ITEMS_CAT :=  "25-29"]
all_ICB_data[ITEMS >= 30 & ITEMS < 35,
             ITEMS_CAT :=  "30-34"]
all_ICB_data[ITEMS >= 35 & ITEMS < 40,
             ITEMS_CAT :=  "35-39"]
all_ICB_data[ITEMS >= 40 & ITEMS < 45,
             ITEMS_CAT :=  "40-44"]
all_ICB_data[ITEMS >= 45 & ITEMS < 50,
             ITEMS_CAT :=  "45-49"]
all_ICB_data[ITEMS >= 50 & ITEMS < 55,
             ITEMS_CAT :=  "50-54"]
all_ICB_data[ITEMS >= 55 & ITEMS < 60,
             ITEMS_CAT :=  "55-59"]
all_ICB_data[ITEMS >= 60 & ITEMS < 65,
             ITEMS_CAT :=  "60-64"]
all_ICB_data[ITEMS >= 65 & ITEMS < 70,
             ITEMS_CAT :=  "65-69"]
all_ICB_data[ITEMS >= 70 & ITEMS < 75,
             ITEMS_CAT :=  "70-74"]
all_ICB_data[ITEMS >= 75 & ITEMS < 80,
             ITEMS_CAT :=  "75-79"]
all_ICB_data[ITEMS >= 80 & ITEMS < 85,
             ITEMS_CAT :=  "80-84"]
all_ICB_data[ITEMS >= 85 & ITEMS < 90,
             ITEMS_CAT :=  "85-89"]
all_ICB_data[ITEMS >= 90 & ITEMS < 95,
             ITEMS_CAT :=  "90-94"]
all_ICB_data[ITEMS >= 95 & ITEMS < 100,
             ITEMS_CAT :=  "95-100"]
all_ICB_data[ITEMS >= 100, ITEMS_CAT :=  "100+"]

# convert to ordered factor
all_ICB_data[,ITEMS_CAT := factor(ITEMS_CAT, levels = 
                                    c("0", "1-4", "5-9", "10-14", 
                                      "15-19", "20-24", "25-29", 
                                      "30-34", "35-39", "40-44",
                                      "45-49", "50-54", "55-59",  
                                      "60-64", "65-69", "70-74",
                                      "75-79", "80-84", "85-89", 
                                      "90-94", "95-100", "100+"))]

# all_ICB_data[, c("YEAR_MONTH", "UNIQUE_PATIENT_COUNT", "ITEMS") := NULL]