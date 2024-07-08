
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

### Need the population to match the age groups of the data, not the slightly off Combo ones. 
pop_sizes_all[AGE_BAND == "0-1", AGE_BAND_COMBO := "<1" ]
pop_sizes_all[AGE_BAND == "2-5", AGE_BAND_COMBO := "1-14" ]
pop_sizes_all[AGE_BAND == "6-10", AGE_BAND_COMBO := "1-14" ]
pop_sizes_all[AGE_BAND == "11-15", AGE_BAND_COMBO := "1-14" ]
pop_sizes_all[AGE_BAND == "16-20", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "21-25", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "26-30", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "31-35", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "36-40", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "41-45", AGE_BAND_COMBO := "15-44" ]
pop_sizes_all[AGE_BAND == "46-50", AGE_BAND_COMBO := "45-64" ]
pop_sizes_all[AGE_BAND == "51-55", AGE_BAND_COMBO := "45-64" ]
pop_sizes_all[AGE_BAND == "56-60", AGE_BAND_COMBO := "45-64" ]
pop_sizes_all[AGE_BAND == "61-65", AGE_BAND_COMBO := "45-64" ]
pop_sizes_all[AGE_BAND == "66-70", AGE_BAND_COMBO := "65-74" ]
pop_sizes_all[AGE_BAND == "71-75", AGE_BAND_COMBO := "65-74" ]
pop_sizes_all[AGE_BAND == "76-80", AGE_BAND_COMBO := "75-84" ]
pop_sizes_all[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
pop_sizes_all[AGE_BAND == "81-85", AGE_BAND_COMBO := "75-84" ]
pop_sizes_all[AGE_BAND == "86+", AGE_BAND_COMBO := "85+" ]
# calculate the pop sizes in the slightly altered new ages
pop_sizes_COMBO <- pop_sizes_all[, sum(value), by = c("AGE_BAND_COMBO", "YEAR", "GENDER")]


# convert monthly to financial year
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
prescrips_annual <- prescrips_annual[!is.na(fyear_start)]
prescrips_annual[pop_sizes_COMBO, on=c(AGE_BAND_COMBO="AGE_BAND_COMBO", fyear_start= "YEAR", GENDER = "GENDER"), population := i.V1]

prescrips_annual[, rate_per_100k := (V1/population)*100000]
# label prescription rate
prescrips_annual[,type := "prescription_rate"]

# # reformat resistance data
# staph_temp <- melt.data.table(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
# colnames(staph_temp)[which(colnames(staph_temp)=="AGE_BAND")] <- "AGE_BAND_COMBO"
# staph_temp[Species == "MRSA", inclusion_label := "Beta-lactam antibiotic"]
# staph_temp[Species == "MSSA", inclusion_label := "Not beta-lactam antibiotic"]
# staph_temp[, date_time := paste0(fyear_start+1,"-04-05") ]
# staph_temp[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d") ]
# staph_temp[, type := "BSI"]
# staph_temp[variable == "CASES_MEN", GENDER := "Male"]
# staph_temp[variable == "CASES_WOMEN", GENDER := "Female"]
# 

### Want to compare against proportion resistant

staph <- fread("data/staph_aur_bacteraemia.csv")
staph[,fyear_start :=as.numeric(substr(FINANCIAL_YEAR, start = 1, stop = 4)) ]
#remove older data
staph <- staph[fyear_start>2014]
staph[, AGE_BAND := factor(AGE_BAND,
                           levels = c("<1","1-14","15-44", "45-64" ,"65-74", "75-84", "85+" ))]
staph_temp <- staph[, c("Species", "fyear_start", "AGE_BAND", "CASES_MEN", "CASES_WOMEN")]
# reformat gender
staph_temp_m <- melt.data.table(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
# cast to seperate columns for MRSA and MSSA
staph_temp_c <- dcast.data.table(staph_temp_m,  AGE_BAND + variable +fyear_start~ Species, value.var = "value")
# calculate proportion
staph_temp_c[, propR := MRSA/(MRSA+MSSA)]
# remove no longer needed colunmns
staph_temp_c[, c("MRSA", "MSSA") := NULL]
# add a type for combining with prescription data
staph_temp_c[, type := "prop_resistant"]
#include only needed columns
staph_temp_c <- staph_temp_c[,c("fyear_start", "variable","AGE_BAND", "propR", "type")]
# check correct labelling
colnames(staph_temp_c)[which(colnames(staph_temp_c)== "AGE_BAND")] <- "AGE_BAND_COMBO"
colnames(staph_temp_c)[which(colnames(staph_temp_c)== "variable")] <- "GENDER"
colnames(staph_temp_c)[which(colnames(staph_temp_c)== "propR")] <- "value"
staph_temp_c[GENDER == "CASES_MEN", GENDER := "Male"]
staph_temp_c[GENDER == "CASES_WOMEN", GENDER := "Female"]


# Calculate prescriptions by finanical year and the nrew age groups
prescrips_annual <- all_data_ex[,  sum(ITEMS), by = c("fyear_start", "GENDER", "AGE_BAND_COMBO", "inclusion_label")]
prescrips_annual <- prescrips_annual[!is.na(fyear_start)]
prescrips_annual[pop_sizes_COMBO, on=c(AGE_BAND_COMBO="AGE_BAND_COMBO", fyear_start= "YEAR", GENDER = "GENDER"), population := i.V1]
# rate
prescrips_annual[, rate_per_100k := (V1/population)*100000]
# label prescription rate
prescrips_annual[,type := "prescription_rate"]
# only wanted beta-lactams
prescrips_annual <- prescrips_annual[inclusion_label== "Beta-lactam antibiotic"]
prescrips_annual[, inclusion_label := NULL]
# relabel as needed 
colnames(prescrips_annual)[which(colnames(prescrips_annual)=="rate_per_100k")] <- "value"
prescrips_annual[, c("population", "V1") := NULL]

all_together <- rbind(staph_temp_c, prescrips_annual)

MRSA_PRESCRIPS <- ggplot(all_together, aes(x = fyear_start, y = value, colour = AGE_BAND_COMBO, linetype = GENDER)) + 
  facet_grid(type~AGE_BAND_COMBO, scales = "free_y") + 
  geom_line() + geom_point(size = 0.4) + theme_bw() + 
  labs(x = "Date", y = "Proportion MRSI (over MRSA and MSSA), Annual beta-lactam prescription rate",
       title = "NOTE: AGE BANDS OUT BY A YEAR OR TWO", colour = "Age Band", linetype = "Sex")





# source RTI.R to get the flu plot
FIG4 <- grid.arrange(FLU_VACC, FLU_CHANGE, MRSA_PRESCRIPS,
             layout_matrix = rbind(c(1,2),
                                   c(3,3)))
ggsave(paste0("plots/Fig4.pdf"), plot = FIG4, 
       width = 20, height = 10)

