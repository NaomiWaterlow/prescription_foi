# ICB populations 2023 formatting 
# Does not include Wales

# read in the population data
pop_dat <- fread("data/ICB_populations.csv")

#restrucutre to age and male_female
pop_dat_m <- melt.data.table(pop_dat, id.vars = c("SICBL_2023_Code", "ICB_2023_Code", "ICB_2023_Name"))
pop_dat_m[, SEX := str_extract(variable, "[aA-zZ]+")]
pop_dat_m[, AGE := as.numeric(str_extract(variable, "(\\d)+"))]
# assign age groups
pop_dat_m[AGE %in% c(0,1), AGE_BAND_NEW := "0-1"] 
pop_dat_m[AGE >= 2 & AGE <=5, AGE_BAND_NEW := "2-5"] 
pop_dat_m[AGE >= 6 & AGE <=10, AGE_BAND_NEW := "6-10"] 
pop_dat_m[AGE >= 11 & AGE <=15, AGE_BAND_NEW := "11-15"] 
pop_dat_m[AGE >= 16 & AGE <=20, AGE_BAND_NEW := "16-20"] 
pop_dat_m[AGE >= 21 & AGE <=25, AGE_BAND_NEW := "21-25"] 
pop_dat_m[AGE >= 26 & AGE <=30, AGE_BAND_NEW := "26-30"] 
pop_dat_m[AGE >= 31 & AGE <=35, AGE_BAND_NEW := "31-35"] 
pop_dat_m[AGE >= 36 & AGE <=40, AGE_BAND_NEW := "36-40"] 
pop_dat_m[AGE >= 41 & AGE <=45, AGE_BAND_NEW := "41-45"] 
pop_dat_m[AGE >= 46 & AGE <=50, AGE_BAND_NEW := "46-50"] 
pop_dat_m[AGE >= 51 & AGE <=55, AGE_BAND_NEW := "51-55"] 
pop_dat_m[AGE >= 56 & AGE <=60, AGE_BAND_NEW := "56-60"] 
pop_dat_m[AGE >= 61 & AGE <=65, AGE_BAND_NEW := "61-65"] 
pop_dat_m[AGE >= 66 & AGE <=70, AGE_BAND_NEW := "66-70"] 
pop_dat_m[AGE >= 71 & AGE <=75, AGE_BAND_NEW := "71-75"] 
pop_dat_m[AGE >= 76 & AGE <=80, AGE_BAND_NEW := "76-80"] 
pop_dat_m[AGE >= 81 & AGE <=85, AGE_BAND_NEW := "81-85"] 
pop_dat_m[AGE >= 86 , AGE_BAND_NEW := "86+"] 
pop_dat_m[, SICBL_2023_Code := NULL]
pop_dat_m[, variable := NULL]
# combine into the categories
pop_dat_combo <- dcast.data.table(pop_dat_m, ICB_2023_Code + ICB_2023_Name + SEX + AGE_BAND_NEW ~., value.var = "value", fun.aggregate = sum)
colnames(pop_dat_combo)[which(colnames(pop_dat_combo) == ".")] <- "PEOPLE"
pop_dat_combo[, ICB_2023_Name := toupper(ICB_2023_Name)]

# add onto ICB_ data and match
ICB_names <- fread("data/ICB_names.csv")
ICB_names <- unique(ICB_names)
colnames(ICB_names) <- c("ICB_2023_Code", "ICB_2023_Name")
pop_dat_combo[ICB_names, on = "ICB_2023_Name", ICB_CODE := i.ICB_2023_Code]
pop_dat_combo[SEX == "F", GENDER := "Female"]
pop_dat_combo[SEX == "M", GENDER := "Male"]


fwrite(pop_dat_combo, "data/ICB_Data/ICB_2023_pops.csv")
