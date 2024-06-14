# MRSA
# beta lactam drug labelling

library(stringr)
library(data.table)
library(ggplot2)

all_ICB_data <- fread("data/ICB_Data/all_ICB_data.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", beta_lactam := BETA_LACTAM]
all_ICB_data[beta_lactam == "Y", beta_lactam_label := "Beta-lactam antibiotic" ]
all_ICB_data[beta_lactam == "N", beta_lactam_label := "Not beta-lactam antibiotic" ]

all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]
# For now, sum over the geographies. NOTE THIS WILL CHANGE LATER

combined_ICBs_blam <- all_ICB_data[, sum(ITEMS),
                              by = c("YEAR_MONTH", "GENDER", "AGE_BAND", "YEAR", "MONTH", "beta_lactam_label")]

#drop gender unknown or indeterminate
combined_ICBs_blam <- combined_ICBs_blam[GENDER == "Female" | GENDER == "Male",]
combined_ICBs_blam[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")]
combined_ICBs_blam[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
combined_ICBs_blam[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")]

ggplot(combined_ICBs_blam, aes(x = date_time2, y = V1, colour = AGE_BAND)) + 
  facet_grid(GENDER ~ beta_lactam_label) + 
  geom_line() + 
  theme_bw() + 
  labs(x = "Date", y = "total prescriptions") 

### Now want to compare against MRSA. 

staph <- fread("data/staph_aur_bacteraemia.csv")
staph[,fyear_start :=as.numeric(substr(FINANCIAL_YEAR, start = 1, stop = 4)) ]
#remove older data
staph <- staph[fyear_start>2014]
staph[, AGE_BAND := factor(AGE_BAND,
                           levels = c("<1","1-14","15-44", "45-64" ,"65-74", "75-84", "85+" ))]
#temp use cases rather than rate as better comparison
staph_temp <- staph[, c("Species", "fyear_start", "AGE_BAND", "CASES_MEN", "CASES_WOMEN")]
staph_temp_m <- melt(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
ggplot(staph_temp_m, aes(x = fyear_start, y =value, colour = AGE_BAND)) + 
  geom_line() + geom_point() + 
  facet_wrap(variable~Species, scales = "free") + 
  theme_bw() + 
  labs(x = "financial year start", y = "number of bacteraemia")



all_ICB_data <- all_ICB_data[!AGE_BAND== "Unknown"]
all_ICB_data[AGE_BAND == "0-1", AGE_BAND_COMBO := "<1" ]
all_ICB_data[AGE_BAND == "2-5", AGE_BAND_COMBO := "1-14" ]
all_ICB_data[AGE_BAND == "6-10", AGE_BAND_COMBO := "1-14" ]
all_ICB_data[AGE_BAND == "11-15", AGE_BAND_COMBO := "1-14" ]
all_ICB_data[AGE_BAND == "16-20", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "21-25", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "26-30", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "31-35", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "36-40", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "41-45", AGE_BAND_COMBO := "15-44" ]
all_ICB_data[AGE_BAND == "46-50", AGE_BAND_COMBO := "45-64" ]
all_ICB_data[AGE_BAND == "51-55", AGE_BAND_COMBO := "45-64" ]
all_ICB_data[AGE_BAND == "56-60", AGE_BAND_COMBO := "45-64" ]
all_ICB_data[AGE_BAND == "61-65", AGE_BAND_COMBO := "45-64" ]
all_ICB_data[AGE_BAND == "66-70", AGE_BAND_COMBO := "65-74" ]
all_ICB_data[AGE_BAND == "71-75", AGE_BAND_COMBO := "65-74" ]
all_ICB_data[AGE_BAND == "76-80", AGE_BAND_COMBO := "75-84" ]
all_ICB_data[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
all_ICB_data[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
all_ICB_data[AGE_BAND == "86-90", AGE_BAND_COMBO := "85+" ]
all_ICB_data[AGE_BAND == "91-95", AGE_BAND_COMBO := "85+" ]
all_ICB_data[AGE_BAND == "96-100", AGE_BAND_COMBO := "85+" ]
all_ICB_data[AGE_BAND == "101-105", AGE_BAND_COMBO := "85+" ]
all_ICB_data[AGE_BAND == "105+", AGE_BAND_COMBO := "85+" ]

combined_ICBs_blamage <- all_ICB_data[, sum(ITEMS),
                                   by = c("YEAR_MONTH", "AGE_BAND_COMBO", "YEAR", "MONTH", "beta_lactam_label")]


combined_ICBs_blamage[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")]
combined_ICBs_blamage[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
combined_ICBs_blamage[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")]
combined_ICBs_blamage[, c("YEAR_MONTH", "YEAR", "MONTH", "date_time") := NULL]
combined_ICBs_blamage[,type := "prescriptions"]
staph_temp[, V1 := CASES_MEN + CASES_WOMEN]
colnames(staph_temp)[which(colnames(staph_temp)=="AGE_BAND")] <- "AGE_BAND_COMBO"
staph_temp[Species == "MRSA", beta_lactam_label := "Beta-lactam antibiotic"]
staph_temp[Species == "MSSA", beta_lactam_label := "Not beta-lactam antibiotic"]
staph_temp[, date_time := paste0(fyear_start+1,"-04-05") ]
staph_temp[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d") ]
staph_temp[, type := "BSI"]
staph_temp_match <- staph_temp[, c("AGE_BAND_COMBO", "beta_lactam_label", "V1", "date_time2", "type")]

combined_staph <- rbind(combined_ICBs_blamage,staph_temp_match)


ggplot(combined_staph, aes(x = date_time2, y = V1, colour = AGE_BAND_COMBO)) + 
  geom_line() + 
  facet_wrap(type~beta_lactam_label, scales= "free_y") + geom_point() + 
  labs(x = "Date", y = "Monthly prescriptions (bottom), Annual BSI cases (top)", 
       title = "BSI vs prescriptions. NOTE: AGE BANDS OUT BY A YEAR OR TWO")


combined_staph[type == "prescriptions", comparator := V1 ]
combined_staph[type == "BSI", comparator := V1*1500 ]
ggplot(combined_staph[beta_lactam_label=="Beta-lactam antibiotic"], aes(x = date_time2, y = comparator, colour = AGE_BAND_COMBO, group = type)) + 
  geom_line() + 
  facet_grid(AGE_BAND_COMBO~beta_lactam_label, scales= "free_y") + geom_point() + 
  labs(x = "Date", y = "", 
       title = "BSI vs prescriptions. NOTE: AGE BANDS OUT BY A YEAR OR TWO")


combined_staph_c <- dcast.data.table(combined_staph, AGE_BAND_COMBO + beta_lactam_label + date_time2 + V1 ~ type, value.var = "V1" )
combined_staph_c[, BSI_comparator := BSI*1500]
ggplot(combined_staph_c[beta_lactam_label=="Beta-lactam antibiotic" & !is.na(prescriptions)], 
       aes(x = date_time2, y = prescriptions, colour = AGE_BAND_COMBO)) + 
  geom_line() + 
  geom_point(data = combined_staph_c[beta_lactam_label=="Beta-lactam antibiotic" & !is.na(BSI)], 
             aes( x = date_time2, y = BSI*1500)) + 
  facet_grid(AGE_BAND_COMBO~., scales = "free_y") + 
  scale_y_continuous(
    "Prescriptions (lines)", 
    sec.axis = sec_axis(~ . /1500, name = "BSI (points)")
  ) + theme_bw() + 
  labs(x = "Date", y = "Prescriptions (lines)", 
       title = "BSI MRSA vs beta-lactam prescriptions over time. NOTE: AGE BANDS OUT BY A YEAR OR TWO")

combined_ICBs_blamage[date_time2 > as.Date("2015-04-05") &
                        date_time2 <= as.Date("2016-04-05"), fyear_start := 2015]
combined_ICBs_blamage[date_time2 > as.Date("2016-04-05") &
                        date_time2 <= as.Date("2017-04-05"), fyear_start := 2016]
combined_ICBs_blamage[date_time2 > as.Date("2017-04-05") &
                        date_time2 <= as.Date("2018-04-05"), fyear_start := 2017]
combined_ICBs_blamage[date_time2 > as.Date("2018-04-05") &
                        date_time2 <= as.Date("2019-04-05"), fyear_start := 2018]
combined_ICBs_blamage[date_time2 > as.Date("2019-04-05") &
                        date_time2 <= as.Date("2020-04-05"), fyear_start := 2019]
combined_ICBs_blamage[date_time2 > as.Date("2020-04-05") &
                        date_time2 <= as.Date("2021-04-05"), fyear_start := 2020]
combined_ICBs_blamage[date_time2 > as.Date("2021-04-05") &
                        date_time2 <= as.Date("2022-04-05"), fyear_start := 2021]
combined_ICBs_blamage[date_time2 > as.Date("2022-04-05") &
                        date_time2 <= as.Date("2023-04-05"), fyear_start := 2022]

annual_prescriptions <- combined_ICBs_blamage[, sum(V1), by = c("AGE_BAND_COMBO", "beta_lactam_label", "type", "fyear_start")]
staph_temp_match2 <- staph_temp[,c("AGE_BAND_COMBO", "beta_lactam_label", "type", "fyear_start", "V1")]

combined_staph2 <- rbind(annual_prescriptions, staph_temp_match2)
combined_staph2_c <- dcast.data.table(combined_staph2, AGE_BAND_COMBO + beta_lactam_label + fyear_start + V1 ~ type, value.var = "V1" )
combined_staph2_c[, BSI_comparator := BSI*10000]


ggplot(combined_staph2_c[beta_lactam_label =="Beta-lactam antibiotic"], 
       aes(x = fyear_start, y = prescriptions, fill = AGE_BAND_COMBO)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(AGE_BAND_COMBO~., scales = "free_y") + 
  geom_point(aes(y = BSI_comparator)) + 
  geom_path(data = combined_staph2_c[beta_lactam_label=="Beta-lactam antibiotic" & !is.na(BSI)], 
            aes(y = BSI_comparator)) + 
  scale_y_continuous(
    "Prescriptions in financial year (bars)", 
    sec.axis = sec_axis(~ . /10000, name = "BSI in financial year(points/lines)")
  ) + theme_bw() + 
  labs(x = "Date", , 
       title = "BSI MRSA vs beta-lactam prescriptions over time. NOTE: AGE BANDS OUT BY A YEAR OR TWO")



### Now want to compare against proportion resistant

staph <- fread("data/staph_aur_bacteraemia.csv")
staph[,fyear_start :=as.numeric(substr(FINANCIAL_YEAR, start = 1, stop = 4)) ]
#remove older data
staph <- staph[fyear_start>2014]
staph[, AGE_BAND := factor(AGE_BAND,
                           levels = c("<1","1-14","15-44", "45-64" ,"65-74", "75-84", "85+" ))]
#temp use cases rather than rate as better comparison
staph_temp <- staph[, c("Species", "fyear_start", "AGE_BAND", "CASES_MEN", "CASES_WOMEN")]
staph_temp[, total_cases := CASES_MEN + CASES_WOMEN]
staph_temp_c <- dcast.data.table(staph_temp, fyear_start + AGE_BAND ~ Species, value.var = "total_cases")
staph_temp_c[, propR := MRSA/(MRSA+MSSA)]

staph_temp_c[, c("MRSA", "MSSA") := NULL]

staph_temp_c[, type := "prop"]
staph_temp_c <- staph_temp_c[,c("fyear_start", "AGE_BAND_COMBO",  "type","value")]
colnames(staph_temp_c) <- c("fyear_start", "AGE_BAND_COMBO", "type","value")


prescriptions <- combined_ICBs_blamage[beta_lactam_label=="Beta-lactam antibiotic", c("fyear_start", "AGE_BAND_COMBO", "V1", "type")]
prescriptions <- prescriptions[,sum(value), by = c("fyear_start", "AGE_BAND_COMBO", "type")]
colnames(prescriptions)[4] <- "value"
prescriptions <- prescriptions[!is.na(fyear_start)]

all_together <- rbind(staph_temp_c, prescriptions)

ggplot(all_together, aes(x = fyear_start, y = value, colour = AGE_BAND_COMBO)) + 
  facet_grid(type~AGE_BAND_COMBO, scales = "free_y") + 
  geom_line() + geom_point() + theme_bw() + 
  labs(x = "Financial year", y = "Proportion MRSA (MRSA/(MRSA+MSSA)),    Number of Prescriptions",
       title = "NOTE: AGE BANDS OUT BY A YEAR OR TWO")
