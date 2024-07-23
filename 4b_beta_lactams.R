############ MRSA comparison ############################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

# load in the data 
#read in the data 
all_data_ex <- fread(paste0("data/",sensitivity_choice,"/all_data_organised_",sensitivity_choice,".csv"))
drugs_lookup <- fread("data/drugs_lookup.csv")
pop_sizes_all <- fread("data/pop_sizes.csv")

##### Beta lactam prescriptions #####

#Match drug name
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
# default don't include as beta lactam
drugs_lookup[, BETA_LACTAM := "N"]
# label as beta-lactam if Penicillin or ceph/b-lactam categories
drugs_lookup[Drug_type %in% c("Penicillins", "Cephalosporins and other beta-lactams"), BETA_LACTAM := "Y"]
# transfer labels over to prescription data
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", inclusion_label := BETA_LACTAM]
# label for clarity
all_data_ex[inclusion_label == "Y", inclusion_label := "Beta-lactam antibiotic"]

# factor age groups
all_data_ex[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86+") )]


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


####### Now want to compare against MRSA. #####

staph <- fread("data/staph_aur_bacteraemia.csv")
#convert to year
staph[,fyear_start :=as.numeric(substr(FINANCIAL_YEAR, start = 1, stop = 4)) ]
#remove older data
staph <- staph[fyear_start>2014]
# foramt ages
staph[, AGE_BAND := factor(AGE_BAND,
                           levels = c("<1","1-14","15-44", "45-64" ,"65-74", "75-84", "85+" ))]
#initially use cases rather than rate to compare
staph_temp <- staph[, c("Species", "fyear_start", "AGE_BAND", "CASES_MEN", "CASES_WOMEN")]
staph_temp_m <- melt(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
ggplot(staph_temp_m, aes(x = fyear_start, y =value, colour = AGE_BAND)) + 
  geom_line() + geom_point() + 
  facet_wrap(variable~Species, scales = "free") + 
  theme_bw() + 
  labs(x = "financial year start", y = "number of bacteraemia")
# hard to compare

# convert prescriptions data to the same-ish age groups as the resistance data
all_data_ex[AGE_BAND == "0-1", AGE_BAND_COMBO := "0-1" ]
all_data_ex[AGE_BAND == "2-5", AGE_BAND_COMBO := "2-15" ]
all_data_ex[AGE_BAND == "6-10", AGE_BAND_COMBO := "2-15" ]
all_data_ex[AGE_BAND == "11-15", AGE_BAND_COMBO := "2-15" ]
all_data_ex[AGE_BAND == "16-20", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "21-25", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "26-30", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "31-35", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "36-40", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "41-45", AGE_BAND_COMBO := "16-45" ]
all_data_ex[AGE_BAND == "46-50", AGE_BAND_COMBO := "46-65" ]
all_data_ex[AGE_BAND == "51-55", AGE_BAND_COMBO := "46-65" ]
all_data_ex[AGE_BAND == "56-60", AGE_BAND_COMBO := "46-65" ]
all_data_ex[AGE_BAND == "61-65", AGE_BAND_COMBO := "46-65" ]
all_data_ex[AGE_BAND == "66-70", AGE_BAND_COMBO := "66-70" ]
all_data_ex[AGE_BAND == "71-75", AGE_BAND_COMBO := "66-70" ]
all_data_ex[AGE_BAND == "76-80", AGE_BAND_COMBO := "76-80" ]
all_data_ex[AGE_BAND == "81-85", AGE_BAND_COMBO := "76-80" ]
all_data_ex[AGE_BAND == "81-85", AGE_BAND_COMBO := "76-80" ]
all_data_ex[AGE_BAND == "86+", AGE_BAND_COMBO := "86+" ]

### Need the population to match the age groups of the data, not the slightly off Combo ones. 
pop_sizes_all[AGE_BAND == "0-1", AGE_BAND_COMBO := "0-1" ]
pop_sizes_all[AGE_BAND == "2-5", AGE_BAND_COMBO := "2-15" ]
pop_sizes_all[AGE_BAND == "6-10", AGE_BAND_COMBO := "2-15" ]
pop_sizes_all[AGE_BAND == "11-15", AGE_BAND_COMBO := "2-15" ]
pop_sizes_all[AGE_BAND == "16-20", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "21-25", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "26-30", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "31-35", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "36-40", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "41-45", AGE_BAND_COMBO := "16-45" ]
pop_sizes_all[AGE_BAND == "46-50", AGE_BAND_COMBO := "46-65" ]
pop_sizes_all[AGE_BAND == "51-55", AGE_BAND_COMBO := "46-65" ]
pop_sizes_all[AGE_BAND == "56-60", AGE_BAND_COMBO := "46-65" ]
pop_sizes_all[AGE_BAND == "61-65", AGE_BAND_COMBO := "46-65" ]
pop_sizes_all[AGE_BAND == "66-70", AGE_BAND_COMBO := "66-70" ]
pop_sizes_all[AGE_BAND == "71-75", AGE_BAND_COMBO := "66-70" ]
pop_sizes_all[AGE_BAND == "76-80", AGE_BAND_COMBO := "76-80" ]
pop_sizes_all[AGE_BAND == "81-85", AGE_BAND_COMBO := "76-80" ]
pop_sizes_all[AGE_BAND == "81-85", AGE_BAND_COMBO := "76-80" ]
pop_sizes_all[AGE_BAND == "86+", AGE_BAND_COMBO := "86+" ]
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


#### Compare against proportion resistant ####

# reformat gender
staph_temp_m <- melt.data.table(staph_temp, id.vars = c("Species", "fyear_start", "AGE_BAND"))
# cast to separate columns for MRSA and MSSA
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
# remove any without finanical yaer
prescrips_annual <- prescrips_annual[!is.na(fyear_start)]
# match population sizes acrpss
prescrips_annual[pop_sizes_COMBO, on=c(AGE_BAND_COMBO="AGE_BAND_COMBO", fyear_start= "YEAR", GENDER = "GENDER"), population := i.V1]
# rate
prescrips_annual[, rate_per_100k := (V1/population)*100000]
# label prescription rate
prescrips_annual[,type := "prescription_rate"]
# only wanted beta-lactams
prescrips_annual <- prescrips_annual[inclusion_label== "Beta-lactam antibiotic"]
# remove unneeded column
prescrips_annual[, inclusion_label := NULL]
# relabel as needed 
colnames(prescrips_annual)[which(colnames(prescrips_annual)=="rate_per_100k")] <- "value"
prescrips_annual[, c("population", "V1") := NULL]

#create column for matching age bands in plot
staph_temp_c[,matched_age := AGE_BAND_COMBO]
prescrips_annual[AGE_BAND_COMBO == "0-1",matched_age := "<1"]
prescrips_annual[AGE_BAND_COMBO == "2-15",matched_age := "1-14"]
prescrips_annual[AGE_BAND_COMBO == "16-45",matched_age := "15-44"]
prescrips_annual[AGE_BAND_COMBO == "46-65",matched_age := "45-64"]
prescrips_annual[AGE_BAND_COMBO == "66-70",matched_age := "65-74"]
prescrips_annual[AGE_BAND_COMBO == "76-80",matched_age := "75-84"]
prescrips_annual[AGE_BAND_COMBO == "86+",matched_age := "85+"]

#combine into one data frame
all_together <- rbind(staph_temp_c, prescrips_annual)

# nice labels
all_together[type == "prescription_rate", nice_labels := "Prescription rate"]
all_together[type == "prop_resistant", nice_labels := "Proportion resistant"]
all_together[type == "prop_resistant", upper_lim := 0.15]
all_together[type == "prescription_rate", upper_lim := 80000]

#covid rectangle geom_rect object
rectangle_object <- unique(all_together[, c("nice_labels", "type","AGE_BAND_COMBO", "upper_lim", "matched_age")])
rectangle_object[,fyear_start := 2019]
rectangle_object[,value := 0]

all_together

MRSA_PRESCRIPS <- ggplot(all_together, aes(x = fyear_start, y = value, colour = GENDER, linetype = nice_labels)) + 
  geom_blank(aes(ymin =0 , ymax = upper_lim) ) +
  geom_rect(data = rectangle_object, xmin = 2019, xmax = 2022, ymin = -0.7, aes( ymax = upper_lim), 
            alpha = 0.4,fill = "grey", colour =NA, inherit.aes = F) +
  facet_wrap(nice_labels~AGE_BAND_COMBO, scales = "free_y", nrow = 2) + 
  geom_line() + geom_point(size = 0.4) + theme_bw() + 
  scale_linetype_manual(values = c(1,2)) + 
  
  labs(x = "Date", y = "Proportion MRSI (over MRSA and MSSA), Annual beta-lactam prescription rate",
       title = "C: BSI resistance vs prescriptions", colour = "Sex", linetype = "Type") 

MRSA_PRESCRIPS



# source RTI.R to get the flu plot
FIG4 <- grid.arrange(FLU_VACC, FLU_CHANGE, MRSA_PRESCRIPS,
             layout_matrix = rbind(c(1,2),
                                   c(3,3)))
ggsave(paste0("plots/",sensitivity_choice,"/Fig4_",sensitivity_choice,".pdf"), plot = FIG4, 
       width = 20, height = 10)

