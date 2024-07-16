############ COMBINE GP ############################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################


# Find all the names of the files
practice_files <- list.files("data/Practice_Data/")
loc <- paste0("data/Practice_Data/", practice_files)
#remove the already combined dataset if rerunning
if("data/Practice_Data/all_practice_data.csv" %in% loc){
  loc <- loc[-which(loc == "data/Practice_Data/all_practice_data.csv")]}
# Combine them all into one data.table
all_practice_data <- rbindlist(lapply(loc, fread), fill = TRUE)

# Check for NAs
which(is.na(all_practice_data))
# No NAS in the the whole dataset

# Split year and month for later use
all_practice_data[, "YEAR" := as.numeric(substr(YEAR_MONTH, 1, 4)),]
all_practice_data[, "MONTH" := as.numeric(substr(YEAR_MONTH, 5, 6)),]

# convert to numeric -> the "*"s get converted to NAs
all_practice_data[, UNIQUE_PATIENT_COUNT := as.numeric(UNIQUE_PATIENT_COUNT)]
# convert to numeric -> the "*"s get converted to NAs
all_practice_data[, ITEMS := as.numeric(ITEMS)]

if(sensitivity_choice == "default_1"){

  all_practice_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]

  all_practice_data[is.na(ITEMS), ITEMS := 1]
  
} else if (sensitivity_choice == "sens_4"){

  all_practice_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 4]

  all_practice_data[is.na(ITEMS), ITEMS := 4]
  
} else if (sensitivity_choice == "m4_f1"){
  
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Male", UNIQUE_PATIENT_COUNT := 4]
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Female", UNIQUE_PATIENT_COUNT := 1]
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Indeterminate", UNIQUE_PATIENT_COUNT := 0]
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Unknown", UNIQUE_PATIENT_COUNT := 0]
  
  all_practice_data[is.na(ITEMS) & 
                 GENDER == "Male", ITEMS := 4]
  all_practice_data[is.na(ITEMS) & 
                 GENDER == "Female", ITEMS := 1]
  all_practice_data[is.na(ITEMS) & 
                 GENDER == "Indeterminate", ITEMS := 0]
  all_practice_data[is.na(ITEMS) & 
                 GENDER == "Unknown", ITEMS := 0]
  
} else if (sensitivity_choice == "20_cutoff"){
  
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
  
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 AGE_BAND %in% c("0-1", "2-5", "6-10", 
                                 "11-15", "16-20"), UNIQUE_PATIENT_COUNT := 4]
  
  all_practice_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 AGE_BAND %in% c("Unknown"), UNIQUE_PATIENT_COUNT := 0]
  
  all_practice_data[is.na(ITEMS), ITEMS := 1]
  
  all_practice_data[is.na(ITEMS) & 
                 AGE_BAND %in% c("0-1", "2-5", "6-10", 
                                 "11-15", "16-20"), ITEMS := 4]
  
  all_practice_data[is.na(ITEMS) & 
                 AGE_BAND %in% c("Unknown"), ITEMS := 0]
  
} else {
  print("Not a valid sensitivity choice")
  stop()
}

all_practice_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]


fwrite(all_practice_data, paste0("data/",sensitivity_choice,"/all_practice_data_", sensitivity_choice,".csv"))

