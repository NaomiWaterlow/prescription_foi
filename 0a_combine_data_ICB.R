
############ COMBINE ICB ############################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

# Find all the names of the files
ICB_files <- list.files("data/ICB_Data/")
loc <- paste0("data/ICB_Data/", ICB_files)
#remove the already combined dataset if rerunning
if("data/ICB_Data/all_ICB_data.csv" %in% loc){
  loc <- loc[-which(loc == "data/ICB_Data/all_ICB_data.csv")]
  loc <- loc[-which(loc == "data/ICB_Data/ICB_2023_pops.csv")]}
# Combine them all into one data.table
all_ICB_data <- rbindlist(lapply(loc, fread), fill = TRUE)

# Check for NAs
which(is.na(all_ICB_data))
# No NAS in the the whole dataset

# Split year and month for later use
all_ICB_data[, "YEAR" := as.numeric(substr(YEAR_MONTH, 1, 4)),]
all_ICB_data[, "MONTH" := as.numeric(substr(YEAR_MONTH, 5, 6)),]

# convert to numeric -> the "*"s get converted to NAs
suppressWarnings(all_ICB_data[, UNIQUE_PATIENT_COUNT := as.numeric(UNIQUE_PATIENT_COUNT)])
# convert to numeric -> the "*"s get converted to NAs
suppressWarnings(all_ICB_data[, ITEMS := as.numeric(ITEMS)])

if(sensitivity_choice == "default_1"){
  # Change stars to 1 so can be used in numeric
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
  # Change stars to 1 so can be used in numeric
  all_ICB_data[is.na(ITEMS), ITEMS := 1]
  
} else if (sensitivity_choice == "sens_4"){
  # Change stars to 4 (max) so can be used in numeric
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 4]
  # Change stars to 4 so can be used in numeric
  all_ICB_data[is.na(ITEMS), ITEMS := 4]
  
} else if (sensitivity_choice == "m4_f1"){
  # Change stars for male patients to 4 so can be used in numeric
  # Taking males to highest level to test extremes seen (women usually more prescribing)
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Male", UNIQUE_PATIENT_COUNT := 4]
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Female", UNIQUE_PATIENT_COUNT := 1]
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Indeterminate", UNIQUE_PATIENT_COUNT := 0]
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 GENDER == "Unknown", UNIQUE_PATIENT_COUNT := 0]
  
  all_ICB_data[is.na(ITEMS) & 
                 GENDER == "Male", ITEMS := 4]
  all_ICB_data[is.na(ITEMS) & 
                 GENDER == "Female", ITEMS := 1]
  all_ICB_data[is.na(ITEMS) & 
                 GENDER == "Indeterminate", ITEMS := 0]
  all_ICB_data[is.na(ITEMS) & 
                 GENDER == "Unknown", ITEMS := 0]
  
} else if (sensitivity_choice == "20_cutoff"){
  # Change stars for younger patients to 4 
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
  
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 AGE_BAND %in% c("0-1", "2-5", "6-10", 
                                 "11-15", "16-20"), UNIQUE_PATIENT_COUNT := 4]
  
  all_ICB_data[is.na(UNIQUE_PATIENT_COUNT) & 
                 AGE_BAND %in% c("Unknown"), UNIQUE_PATIENT_COUNT := 0]
  
  all_ICB_data[is.na(ITEMS), ITEMS := 1]
  
  all_ICB_data[is.na(ITEMS) & 
                 AGE_BAND %in% c("0-1", "2-5", "6-10", 
                                 "11-15", "16-20"), ITEMS := 4]
  
  all_ICB_data[is.na(ITEMS) & 
                 AGE_BAND %in% c("Unknown"), ITEMS := 0]
  
} else {
  print("Not a valid sensitivity choice")
  stop()
}

# Get age band ordering correct
all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]


fwrite(all_ICB_data, paste0("data/",sensitivity_choice,"/all_ICB_data_", sensitivity_choice,".csv"))

