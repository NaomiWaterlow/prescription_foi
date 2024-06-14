# GBS case study

all_ICB_data <- fread("data/ICB_Data/all_ICB_data.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]



all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]

# For now, sum over the geographies, genders, years,
data_for_GBS <- all_ICB_data[, sum(ITEMS),
                                   by = c("YEAR", "AGE_BAND", "YEAR", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE")]
data_for_GBS[drug_name == "Tetracycline", target_drug := "Tetracycline"]
data_for_GBS[drug_name %in% c("Clindamycin hydrochloride", "Clindamycin phosphate"), target_drug := "Clindamycin"]
data_for_GBS[drug_name %in% c("Erythromycin", "Erythromycin ethylsuccinate", 
                              "Erythromycin lactobionate", "Erythromycin stearate"), target_drug := "Erythromycin"]

data_for_GBS <- data_for_GBS[!(is.na(target_drug))]
data_for_GBS[ , age_binary := "Other"]
data_for_GBS[ AGE_BAND == "0-1", age_binary := "<1"]

data_for_GBS <- data_for_GBS[, sum(V1), by = c("YEAR", "target_drug", "age_binary")]
all_value <- data_for_GBS[, sum(V1), by = c("YEAR", "target_drug")]

data_for_GBS[all_value, on = c("YEAR", "target_drug" ), prescriptions := i.V1]
data_for_GBS[age_binary== "<1", prescriptions := V1]

data_for_GBS[age_binary == "Other", age_binary := "All"]

colnames(data_for_GBS) <- c("Year", "target_drug", "age_binary","V1", "value")

# read in the proportion data
GBS_resistant <- fread("data/GBS_resistance.csv")
GBS_resistant_m <- melt.data.table(GBS_resistant, id.vars = c("Year", "AGE"))
colnames(GBS_resistant_m) <- c("Year", "age_binary", "target_drug", "value")
GBS_resistant_m <- GBS_resistant_m[,c("Year", "target_drug", "age_binary", "value")]
data_for_GBS[,"V1" := NULL]
data_for_GBS[, type := "Prescriptions"]
GBS_resistant_m[, type := "Proportion"]

combined_GBS <- rbind(data_for_GBS, GBS_resistant_m)


ggplot(combined_GBS, aes(x = Year, y = value, colour = target_drug)) + 
  geom_line() + geom_point()+
  facet_wrap(age_binary~type, scales = "free_y") + 
  labs(x = "Year", y = " Proportion GBS resistant (bottom), Number of prescriptions (top)", 
       title = "NOTE: prescriptions are actually age 0 and 1, not <1") + 
  theme_bw()






