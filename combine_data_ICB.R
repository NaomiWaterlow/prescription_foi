library(data.table)

## ICB level - combine and reformat data
# Note, this takes ages. Very big dataset.
#####

# Find all the names of the files
ICB_files <- list.files("data/ICB_Data/")
loc <- paste0("data/ICB_Data/", ICB_files)
#remove the already combined dataset if rerunning
if("data/ICB_Data/all_ICB_data.csv" %in% loc){
loc <- loc[-which(loc == "data/ICB_Data/all_ICB_data.csv")]}
# Combine them all into one data.table
all_ICB_data <- rbindlist(lapply(loc, fread), fill = TRUE)

# Check for NAs
which(is.na(all_ICB_data))
# No NAS in the the whole dataset

# Split year and month for later use
all_ICB_data[, "YEAR" := as.numeric(substr(YEAR_MONTH, 1, 4)),]
all_ICB_data[, "MONTH" := as.numeric(substr(YEAR_MONTH, 5, 6)),]

# convert to numeric -> the "*"s get converted to NAs
all_ICB_data[, UNIQUE_PATIENT_COUNT := as.numeric(UNIQUE_PATIENT_COUNT)]
# Change stars to -1 so can be used in numeric
all_ICB_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := -100]
# convert to numeric -> the "*"s get converted to NAs
all_ICB_data[, ITEMS := as.numeric(ITEMS)]
# Change stars to -1 so can be used in numeric
all_ICB_data[is.na(ITEMS), ITEMS := -100]


all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                                     "6-10", "11-15", "16-20", "21-25", 
                                                                     "26-30", "31-35", "36-40", "41-45", 
                                                                     "46-50", "51-55",  "56-60", "61-65", 
                                                                     "66-70", "71-75", "76-80",  "81-85" ,
                                                                     "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]


fwrite(all_ICB_data, "data/ICB_Data/all_ICB_data.csv")

