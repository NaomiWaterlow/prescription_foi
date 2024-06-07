# ICB populations. 
library(stringr)

# FOR GEOGRAPHIC VARIATION
# - excludes Wales
# - 2023 only
# - combines age-groups over 85 into one age group
# - excludes all Unknown categories (3.3%) as we don't know what the pop size for these is
# - excludes "Indeterminate" Gender, as don't know population size.

# Because need to combine age bands, have excluded UNIQUE_PATIENT_COLUMN as we can't use this without risking double counting


# looking only at 2023 'post-covid' as this is also the year we have population data for
# not including wales (For now), as the population size dataset doesn't include it
# combine age_groups over 85
ICB_data_2023 <- all_ICB_data[YEAR==2023 & ICB_CODE != "Q99" & 
                                GENDER != "Unknown" & AGE_BAND != "Unknown" &
                                GENDER != "Indeterminate"]
ICB_data_2023[, UNIQUE_PATIENT_COUNT := NULL]

ICB_data_2023[, AGE_BAND_NEW := AGE_BAND]
ICB_data_2023[AGE_BAND == "86-90", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "91-95", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "96-100", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "101-105", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "105+", AGE_BAND_NEW := "86+"]

ICB_data_2023 <- dcast.data.table(ICB_data_2023, YEAR_MONTH + ICB_CODE + BNF_CHEMICAL_SUBSTANCE_CODE + GENDER + YEAR + MONTH + AGE_BAND_NEW~. , 
                 value.var = "ITEMS", fun.aggregate = sum)
colnames(ICB_data_2023)[which(colnames(ICB_data_2023) == ".")] <- "ITEMS"

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

# add the population data
ICB_data_2023[pop_dat_combo, on = c("ICB_CODE", "AGE_BAND_NEW", "GENDER"), POP :=i.PEOPLE]
ICB_data_2023[, rate_per_1000 := (ITEMS/POP)*1000]

# add in drug lookup 
drugs_lookup <- fread("data/drugs_lookup.csv")
ICB_data_2023[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]

# save these for each drug

# Lets take a look!

# only looking at drugs where there were at least 500 prescriptions in the year
drugs_of_interest <- ICB_data_2023[,sum(ITEMS), by = "BNF_CHEMICAL_SUBSTANCE_CODE"][V1 >=500]$BNF_CHEMICAL_SUBSTANCE_CODE

for(i in drugs_of_interest){
target <- i
target_name <- ICB_data_2023[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
ICB_target_2023 <- ICB_data_2023[BNF_CHEMICAL_SUBSTANCE_CODE == target]

# average per month
ICB_target_2023[, av_rate_per_1000:= mean(rate_per_1000), by = c("ICB_CODE", "GENDER", "AGE_BAND_NEW" )]
ICB_target_2023_av_month <- ICB_target_2023[, c("YEAR_MONTH", "MONTH", "ITEMS", "rate_per_1000", "POP") := NULL]


PLOT_TEMP <- ggplot(ICB_target_2023_av_month, aes(x = AGE_BAND_NEW, y = av_rate_per_1000, colour = ICB_CODE, group =ICB_CODE)) + 
  geom_line() + facet_wrap(GENDER~.) + 
  labs(title = paste0(target_name, " (colours = ICBs)"), x = "Age Group", y = "Average monthly rate per 1000 people") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size =10), 
        legend.position = "none")

ggsave(paste0("plots/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_av_monhtly_rate_items.pdf"), plot = PLOT_TEMP)

}


#### change to look at seasonaility
# average acrosss ICB
ICB_data_2023[, av_across_ICBs := mean(rate_per_1000),by = c("MONTH", "GENDER", "AGE_BAND_NEW", "BNF_CHEMICAL_SUBSTANCE_CODE", "drug_name")]
ICB_data_2023_avICB <- unique(ICB_data_2023[,c("YEAR_MONTH", "ICB_CODE", "ITEMS", "rate_per_1000", "POP") := NULL])



for(i in drugs_of_interest){
  target <- i
  target_name <- ICB_data_2023_avICB[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  ICB_target_2023_avICB <- ICB_data_2023_avICB[BNF_CHEMICAL_SUBSTANCE_CODE == target]

PLOT_TEMP <- ggplot(ICB_target_2023_avICB, aes(x = MONTH, y = av_across_ICBs, colour = AGE_BAND_NEW, group =AGE_BAND_NEW)) + 
  geom_line() + facet_wrap(GENDER~.) + 
  labs(title = paste0(target_name), x = "MONTH", y = "prescription rate per 1000 people, averaged across ICBs") + 
  theme_bw() 

ggsave(paste0("plots/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_seasonality.pdf"), plot = PLOT_TEMP)

}







