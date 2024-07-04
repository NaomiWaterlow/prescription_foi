
####### NOTE!!!! THERE IS SOMETHING WRONG WITH POPULATIOUN DENOMINATORS IN THIS SCRIPT





# MRSA
# beta lactam drug labelling

# required packages
library(stringr)
library(data.table)
library(ggplot2)
# load in the data and mark those that are beta lactams
all_data_ex <- fread("data/all_data_organised.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
pop_sizes_all <- fread("data/pop_sizes.csv")
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", beta_lactam := BETA_LACTAM]
all_data_ex[beta_lactam == "Y", inclusion_label := "Beta-lactam antibiotic" ]
all_data_ex[beta_lactam == "N", inclusion_label := "Not beta-lactam antibiotic" ]
# factor age groups
all_data_ex[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86+") )]


### INterlude, see which antibiotics driving post-covid bump

interlude <- all_data_ex[beta_lactam == "Y" & AGE_BAND == "0-1"]
# sum over gender
interlude_s <- interlude[, sum(ITEMS), by = c("date_time2","drug_name")]
#divide by average
interlude_s[, av_drug := mean(V1), by = c("drug_name")]
interlude_s[, relative := V1/av_drug]

ggplot(interlude_s[date_time2>as.Date("2020-01-01")], aes( x = date_time2, y = relative, colour = drug_name)) + 
  geom_line() + theme_bw() + 
  labs(x = "Date (cut off to start at 2020)", y = "Relative #prescriptions cf average over time period", 
       title = "Which drugs responsible for uptick in 2023? Showing age 0-1 only")



# Combine the number of ITEMS across drug type (e.g. beta lactam or not) - don't need to change population 
combined_subgroup <- all_data_ex[, sum(ITEMS),
                              by = c("date_time2", "GENDER", "AGE_BAND", "YEAR", "MONTH", "inclusion_label", "population")]
# work out items per 100k 
combined_subgroup[, per_100k := (V1/population)*100000]

# colours
cc <- scales::seq_gradient_pal("blue", "darkorange", "Lab")(seq(0,1,length.out=length(unique(all_data_ex$AGE_BAND))))

# have a look at the prescrptions over time
ggplot(combined_subgroup, aes(x = date_time2, y = per_100k, colour = AGE_BAND)) + 
  facet_grid(GENDER ~ inclusion_label) + 
  geom_line() + 
  theme_bw() + 
  labs(x = "Date", y = "Prescriptions per 100k") + 
  scale_colour_manual(values = cc)


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


# convert prescriptions data to the same-ish age groups as the resistance data
all_data_ex[AGE_BAND == "0-1", AGE_BAND_COMBO := "<1" ]
all_data_ex[AGE_BAND == "2-5", AGE_BAND_COMBO := "1-14" ]
all_data_ex[AGE_BAND == "6-10", AGE_BAND_COMBO := "1-14" ]
all_data_ex[AGE_BAND == "11-15", AGE_BAND_COMBO := "1-14" ]
all_data_ex[AGE_BAND == "16-20", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "21-25", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "26-30", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "31-35", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "36-40", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "41-45", AGE_BAND_COMBO := "15-44" ]
all_data_ex[AGE_BAND == "46-50", AGE_BAND_COMBO := "45-64" ]
all_data_ex[AGE_BAND == "51-55", AGE_BAND_COMBO := "45-64" ]
all_data_ex[AGE_BAND == "56-60", AGE_BAND_COMBO := "45-64" ]
all_data_ex[AGE_BAND == "61-65", AGE_BAND_COMBO := "45-64" ]
all_data_ex[AGE_BAND == "66-70", AGE_BAND_COMBO := "65-74" ]
all_data_ex[AGE_BAND == "71-75", AGE_BAND_COMBO := "65-74" ]
all_data_ex[AGE_BAND == "76-80", AGE_BAND_COMBO := "75-84" ]
all_data_ex[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
all_data_ex[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
all_data_ex[AGE_BAND == "86+", AGE_BAND_COMBO := "85+" ]

# Re-calculate the prescriptiosn grouped in the new age groups
# and across financial year

all_data_ex[date_time2 > as.Date("2015-04-05") &
                        date_time2 <= as.Date("2016-04-05"), fyear_start := 2015]
all_data_ex[date_time2 > as.Date("2016-04-05") &
                        date_time2 <= as.Date("2017-04-05"), fyear_start := 2016]
all_data_ex[date_time2 > as.Date("2017-04-05") &
                        date_time2 <= as.Date("2018-04-05"), fyear_start := 2017]
all_data_ex[date_time2 > as.Date("2018-04-05") &
                        date_time2 <= as.Date("2019-04-05"), fyear_start := 2018]
all_data_ex[date_time2 > as.Date("2019-04-05") &
                        date_time2 <= as.Date("2020-04-05"), fyear_start := 2019]
all_data_ex[date_time2 > as.Date("2020-04-05") &
                        date_time2 <= as.Date("2021-04-05"), fyear_start := 2020]
all_data_ex[date_time2 > as.Date("2021-04-05") &
                        date_time2 <= as.Date("2022-04-05"), fyear_start := 2021]
all_data_ex[date_time2 > as.Date("2022-04-05") &
                        date_time2 <= as.Date("2023-04-05"), fyear_start := 2022]


# Sum items across age group and year
prescrips_annual <- all_data_ex[,  sum(ITEMS), by = c("fyear_start", "GENDER", "AGE_BAND_COMBO", "inclusion_label")]

pop <- all_data_ex[,   sum(population),
            by = c("date_time2", "GENDER", "AGE_BAND_COMBO", "inclusion_label", "fyear_start", "drug_name")]
pop[month(date_time2)== 10, ID_year := paste0(fyear_start, "_10")]
pop_sub <- pop[!is.na(ID_year)&!is.na(fyear_start)]
pop <- unique(pop_sub[,c("inclusion_label", "drug_name") := NULL])




data_for_mrsa_comp <- all_data_ex[, c("GENDER", "date_time2", "inclusion_label", "AGE_BAND_COMBO", "ITEMS_NEW", "POPULATION_NEW")]
data_for_mrsa_comp <- unique(data_for_mrsa_comp)



data_for_mrsa_comp[, V1 := (ITEMS_NEW/POPULATION_NEW)*100000]
# label prescription rate
data_for_mrsa_comp[,type := "prescription_rate"]

# reformat resistance data
staph_temp <- melt.data.table(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
colnames(staph_temp)[which(colnames(staph_temp)=="AGE_BAND")] <- "AGE_BAND_COMBO"
staph_temp[Species == "MRSA", inclusion_label := "Beta-lactam antibiotic"]
staph_temp[Species == "MSSA", inclusion_label := "Not beta-lactam antibiotic"]
staph_temp[, date_time := paste0(fyear_start+1,"-04-05") ]
staph_temp[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d") ]
staph_temp[, type := "BSI"]
staph_temp[variable == "CASES_MEN", GENDER := "Male"]
staph_temp[variable == "CASES_WOMEN", GENDER := "Female"]
# add the population size for the rates too
staph_temp[,YEAR := fyear_start]
data_for_mrsa_comp[, YEAR := year(date_time2)]
staph_temp[data_for_mrsa_comp,on = c("YEAR", "GENDER", "AGE_BAND_COMBO"), population := POPULATION_NEW]
staph_temp[, V1 := (value/population)*1000000]
staph_temp_match <- staph_temp[, c("GENDER", "date_time2","inclusion_label","AGE_BAND_COMBO", "V1", "type")]

#format for combining
data_for_mrsa_comp[, c("ITEMS_NEW", "POPULATION_NEW", "YEAR") := NULL]
data_for_mrsa_comp$date_time2 <- as.Date(data_for_mrsa_comp$date_time2)

combined_staph <- rbind(data_for_mrsa_comp,staph_temp_match)
combined_staph <- combined_staph[!is.na(inclusion_label)]

# PLOT THE RATE OF PRESCRIPTIONS VS NOT
ggplot(combined_staph[inclusion_label == "Beta-lactam antibiotic"], aes(x = date_time2, y = V1, colour = AGE_BAND_COMBO, group = interaction(GENDER, AGE_BAND_COMBO))) + 
  geom_line() + 
  facet_grid(type~GENDER, scales= "free_y") + geom_point() + 
  labs(x = "Date", y = "Monthly prescriptions per 100k (bottom), Annual BSI cases per 1 mil (top)", 
       title = "BSI rates vs prescription rates. NOTE: AGE BANDS OUT BY A YEAR OR TWO") + 
  theme_bw()


combined_staph_c <- dcast.data.table(combined_staph, AGE_BAND_COMBO + inclusion_label + date_time2 + V1 + GENDER ~ type, value.var = "V1" )

ggplot(combined_staph_c[inclusion_label=="Beta-lactam antibiotic" & !is.na(prescription_rate)], 
       aes(x = date_time2, y = prescription_rate, colour = GENDER)) + 
  geom_line() + 
  geom_point(data = combined_staph_c[inclusion_label=="Beta-lactam antibiotic" & !is.na(BSI)], 
             aes( x = date_time2, y = BSI*1500)) + 
  facet_grid(AGE_BAND_COMBO~., scales = "free_y") + 
  scale_y_continuous(
    "Prescriptions (lines)", 
    sec.axis = sec_axis(~ . /1500, name = "BSI (points)")
  ) + theme_bw() + 
  labs(x = "Date", y = "Prescriptions (lines)", 
       title = "BSI MRSA rates vs beta-lactam prescription rate over time. NOTE: AGE BANDS OUT BY A YEAR OR TWO")

# data_for_mrsa_comp[date_time2 > as.Date("2015-04-05") &
#                         date_time2 <= as.Date("2016-04-05"), fyear_start := 2015]
# data_for_mrsa_comp[date_time2 > as.Date("2016-04-05") &
#                         date_time2 <= as.Date("2017-04-05"), fyear_start := 2016]
# data_for_mrsa_comp[date_time2 > as.Date("2017-04-05") &
#                         date_time2 <= as.Date("2018-04-05"), fyear_start := 2017]
# data_for_mrsa_comp[date_time2 > as.Date("2018-04-05") &
#                         date_time2 <= as.Date("2019-04-05"), fyear_start := 2018]
# data_for_mrsa_comp[date_time2 > as.Date("2019-04-05") &
#                         date_time2 <= as.Date("2020-04-05"), fyear_start := 2019]
# data_for_mrsa_comp[date_time2 > as.Date("2020-04-05") &
#                         date_time2 <= as.Date("2021-04-05"), fyear_start := 2020]
# data_for_mrsa_comp[date_time2 > as.Date("2021-04-05") &
#                         date_time2 <= as.Date("2022-04-05"), fyear_start := 2021]
# data_for_mrsa_comp[date_time2 > as.Date("2022-04-05") &
#                         date_time2 <= as.Date("2023-04-05"), fyear_start := 2022]

# annual_prescriptions <- data_for_mrsa_comp[, sum(V1), by = c("AGE_BAND_COMBO", "beta_lactam_label", "type", "fyear_start")]
# staph_temp_match2 <- staph_temp[,c("AGE_BAND_COMBO", "beta_lactam_label", "type", "fyear_start", "V1")]
# 
# combined_staph2 <- rbind(annual_prescriptions, staph_temp_match2)
# combined_staph2_c <- dcast.data.table(combined_staph2, AGE_BAND_COMBO + beta_lactam_label + fyear_start + V1 ~ type, value.var = "V1" )
# combined_staph2_c[, BSI_comparator := BSI*10000]


# ggplot(combined_staph2_c[beta_lactam_label =="Beta-lactam antibiotic"], 
#        aes(x = fyear_start, y = prescriptions, fill = AGE_BAND_COMBO)) + 
#   geom_bar(stat = "identity", position = "dodge") + 
#   facet_grid(AGE_BAND_COMBO~., scales = "free_y") + 
#   geom_point(aes(y = BSI_comparator)) + 
#   geom_path(data = combined_staph2_c[beta_lactam_label=="Beta-lactam antibiotic" & !is.na(BSI)], 
#             aes(y = BSI_comparator)) + 
#   scale_y_continuous(
#     "Prescriptions in financial year (bars)", 
#     sec.axis = sec_axis(~ . /10000, name = "BSI in financial year(points/lines)")
#   ) + theme_bw() + 
#   labs(x = "Date", , 
#        title = "BSI MRSA vs beta-lactam prescriptions over time. NOTE: AGE BANDS OUT BY A YEAR OR TWO")



### Now want to compare against proportion resistant

staph <- fread("data/staph_aur_bacteraemia.csv")
staph[,fyear_start :=as.numeric(substr(FINANCIAL_YEAR, start = 1, stop = 4)) ]
#remove older data
staph <- staph[fyear_start>2014]
staph[, AGE_BAND := factor(AGE_BAND,
                           levels = c("<1","1-14","15-44", "45-64" ,"65-74", "75-84", "85+" ))]
#temp use cases rather than rate as better comparison
staph_temp <- staph[, c("Species", "fyear_start", "AGE_BAND", "CASES_MEN", "CASES_WOMEN")]

staph_temp[, date_time := paste0(fyear_start+1,"-04-05") ]
staph_temp[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d") ]

staph_temp_m <- melt.data.table(staph_temp, id.vars = c("Species", "date_time2","date_time", "fyear_start", "AGE_BAND"))
staph_temp_c <- dcast.data.table(staph_temp_m, date_time2 + AGE_BAND + variable ~ Species, value.var = "value")
staph_temp_c[, propR := MRSA/(MRSA+MSSA)]

staph_temp_c[, c("MRSA", "MSSA") := NULL]

staph_temp_c[, type := "prop_resistant"]
staph_temp_c <- staph_temp_c[,c("date_time2", "AGE_BAND", "variable", "type","propR")]

colnames(staph_temp_c) <- c("date_time2", "AGE_BAND_COMBO","GENDER", "type","value")
staph_temp_c[GENDER == "CASES_MEN", GENDER := "Male"]
staph_temp_c[GENDER == "CASES_WOMEN", GENDER := "Female"]

prescriptions <- data_for_mrsa_comp[inclusion_label=="Beta-lactam antibiotic", c("date_time2", "AGE_BAND_COMBO", "GENDER", "type","V1")]
colnames(prescriptions)[which(colnames(prescriptions)=="V1")] <- "value"
prescriptions[, date_time2 := as.Date(date_time2)]
prescriptions[date_time2 > as.Date("2015-04-05") &
                        date_time2 <= as.Date("2016-04-05"), fyear_start := 2015]
prescriptions[date_time2 > as.Date("2016-04-05") &
                        date_time2 <= as.Date("2017-04-05"), fyear_start := 2016]
prescriptions[date_time2 > as.Date("2017-04-05") &
                        date_time2 <= as.Date("2018-04-05"), fyear_start := 2017]
prescriptions[date_time2 > as.Date("2018-04-05") &
                        date_time2 <= as.Date("2019-04-05"), fyear_start := 2018]
prescriptions[date_time2 > as.Date("2019-04-05") &
                        date_time2 <= as.Date("2020-04-05"), fyear_start := 2019]
prescriptions[date_time2 > as.Date("2020-04-05") &
                        date_time2 <= as.Date("2021-04-05"), fyear_start := 2020]
prescriptions[date_time2 > as.Date("2021-04-05") &
                        date_time2 <= as.Date("2022-04-05"), fyear_start := 2021]
prescriptions[date_time2 > as.Date("2022-04-05") &
                        date_time2 <= as.Date("2023-04-05"), fyear_start := 2022]
prescriptions_annual <- prescriptions[, sum(value), by = c("AGE_BAND_COMBO", "GENDER", "type","fyear_start" )]
prescriptions_annual <- prescriptions_annual[!is.na(fyear_start)]
colnames(prescriptions_annual)[which(colnames(prescriptions_annual)=="V1")] <- "value"
prescriptions_annual[, date_time2 := as.Date(paste0(fyear_start, "-04-05"))]
prescriptions_annual[,fyear_start:=NULL]

all_together <- rbind(staph_temp_c, prescriptions_annual)

MRSA_PRESCRIPS <- ggplot(all_together, aes(x = date_time2, y = value, colour = AGE_BAND_COMBO, linetype = GENDER)) + 
  facet_grid(type~AGE_BAND_COMBO, scales = "free_y") + 
  geom_line() + geom_point(size = 0.4) + theme_bw() + 
  labs(x = "Date", y = "Proportion MRSI (over MRSA and MSSA), Annual prescriptions",
       title = "NOTE: AGE BANDS OUT BY A YEAR OR TWO", colour = "Age Band", linetype = "Sex")

# multiplier <- 1000000
# 
# all_together[type == "prop_resistant", value := value*multiplier]
# 
# MRSA_PRESCRIPS2 <- ggplot(all_together, aes(x = date_time2, y = value, colour = GENDER, linetype = type, group = interaction(type, GENDER))) + 
#   facet_grid(.~AGE_BAND_COMBO, scales = "free_y") + 
#   geom_line() + theme_bw() +
#   labs(x = "Financial Year",
#        title = "NOTE: AGE BANDS OUT BY A YEAR OR TWO", colour = "Age Band", linetype = "Sex") + 
#   scale_linetype_manual(values = c(2,1)) + 
#   scale_y_continuous(
#       "Annual prescriptions of beta-lactam antibiotics",
#       sec.axis = sec_axis(~ . /multiplier, name = "Proportion of MRSA BSI (cf MRSA+MSSA BSI)")
#     )
# 



# source RTI.R to get the flu plot
grid.arrange(MRSA_PRESCRIPS, FLU_CHANGE, ncol =2)
