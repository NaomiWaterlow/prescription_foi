
############ Respiratory Tract Infections Analysis ############################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

# Read in data
all_data_ex <- fread(paste0("data/",sensitivity_choice,"/all_data_organised_",sensitivity_choice,".csv"))
drugs_lookup <- fread("data/drugs_lookup.csv")
pop_sizes_all <- fread("data/pop_sizes.csv")
#Match drug name
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
# default don't include as RTI
drugs_lookup[, RTI := "N"]
# label as RTI if Penicillin or ceph/b-lactam categories
drugs_lookup[Drug_type %in% c("Penicillins", "Cephalosporins and other beta-lactams"), RTI := "Y"]
# transfer labels over to prescription data
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", RTI := RTI]
# remove amoxicillin
all_data_ex[drug_name == "Amoxicillin", RTI := "N"]

# sum across genders as not interested. 
data_for_RTI<- all_data_ex[, sum(ITEMS),
                             by = c("YEAR","date_time2", "AGE_BAND", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE", "RTI")]

# keep only respiratory related drugs
data_for_RTI <- data_for_RTI[RTI == "Y"]

# reformat to relevant age bands
data_for_RTI <- data_for_RTI[!is.na(AGE_BAND)]
data_for_RTI[, AGE_BAND_NEW := "15-50"]
data_for_RTI[AGE_BAND == "0-1" , AGE_BAND_NEW := "0-1"]
data_for_RTI[AGE_BAND == "2-5" , AGE_BAND_NEW := "2-5"]
data_for_RTI[AGE_BAND == "6-10" , AGE_BAND_NEW := "6-10"]
data_for_RTI[AGE_BAND == "11-15" , AGE_BAND_NEW := "11-15"]
data_for_RTI[AGE_BAND == "51-55" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND == "56-60" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND == "61-65" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND %in% c("66-70", "71-75", "76-80",  "81-85" ,
             "86-90", "91-95","96-100","101-105", "105+"), AGE_BAND_NEW := "65+"]

# combine across new age bands and drugs. 
temp <- data_for_RTI[, sum(V1), by = c("date_time2", "AGE_BAND_NEW", "RTI")]

# Calculating population sizes to match
pop_sizes_nogender <- pop_sizes_all[, sum(value), by = c("AGE_BAND", "YEAR")]
pop_sizes_nogender <- pop_sizes_nogender[!is.na(AGE_BAND)]
pop_sizes_nogender[, AGE_BAND_NEW := "15-50"]
pop_sizes_nogender[AGE_BAND == "0-1" , AGE_BAND_NEW := "0-1"]
pop_sizes_nogender[AGE_BAND == "2-5" , AGE_BAND_NEW := "2-5"]
pop_sizes_nogender[AGE_BAND == "6-10" , AGE_BAND_NEW := "6-10"]
pop_sizes_nogender[AGE_BAND == "11-15" , AGE_BAND_NEW := "11-15"]
pop_sizes_nogender[AGE_BAND == "51-55" , AGE_BAND_NEW := "50-65"]
pop_sizes_nogender[AGE_BAND == "56-60" , AGE_BAND_NEW := "50-65"]
pop_sizes_nogender[AGE_BAND == "61-65" , AGE_BAND_NEW := "50-65"]
pop_sizes_nogender[AGE_BAND %in% c("66-70", "71-75", "76-80",  "81-85" ,
                             "86-90", "91-95","96-100","101-105", "105+"), AGE_BAND_NEW := "65+"]
pop_sizes_nogender <- pop_sizes_nogender[, sum(V1), by = c("AGE_BAND_NEW", "YEAR")]
# change to date
temp[, YEAR := year(date_time2)]
# add population sizes
temp[pop_sizes_nogender, on = c("YEAR", "AGE_BAND_NEW"), population := i.V1]
# check format
temp[,AGE_BAND_NEW := factor(AGE_BAND_NEW, levels = c("0-1"  , "2-5"  , "6-10",  "11-15", "15-50", "50-65" ,"65+"  ))]
# calculate prescriptions per 100k population
temp[, prescriptions_p_100k := (V1/population)*100000]

# plot
ggplot(temp, aes(x = date_time2, y = prescriptions_p_100k, colour = AGE_BAND_NEW)) + 
  geom_line() + theme_bw() + 
  labs(x = "Date", y = "Prescription rate of RTI antibiotics", 
       title = "Vertical lines indicate when flu vaccination in that age group was implemented", 
       colour = "Age band") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2015-10-01")+30, y = 300, label = "Ages 5-7", angle = 90) +
geom_vline(xintercept = as.Date("2016-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2016-10-01")+30, y = 300, label = "Ages 7-8", angle = 90)+
geom_vline(xintercept = as.Date("2017-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2017-10-01")+30, y = 300, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = as.Date("2018-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2018-10-01")+30, y = 300, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2019-10-01")+30, y = 300, label = "Ages 10-11", angle = 90)+
geom_vline(xintercept = as.Date("2020-10-01")) + 
  annotate("text", x = as.Date("2020-10-01")+30, y = 300, label = "Ages 11-12 & 50-65", angle = 90)+
geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2021-10-01")+30, y = 300, label = "Ages 12-16", angle = 90) 

# normalise compared to 15-50 age group at each time step
temp2 <- temp[AGE_BAND_NEW=="15-50"]
temp[temp2, on = c("date_time2"), mid_age := i.prescriptions_p_100k]
temp[, normalised_rate := prescriptions_p_100k/mid_age]

FLU_VACC <- ggplot(temp, aes(x = date_time2, y = normalised_rate, colour = AGE_BAND_NEW)) + 
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-07-01"), ymin = 0, ymax = 4,
           alpha = 0.4,fill = "grey") +
  geom_line() + theme_bw() + 
  labs(x = "Date", y = "Relative prescirption rate of RTI antibiotics, cf 15-50 age group", 
       title = "A: RTI antibiotics, excluding Amoxicillin", 
       colour = "Age band") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2015-10-01")+30, y = 3.5, label = "Ages 5-7", angle = 90) +
  geom_vline(xintercept = as.Date("2016-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2016-10-01")+30, y = 3.5, label = "Ages 7-8", angle = 90)+
  geom_vline(xintercept = as.Date("2017-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2017-10-01")+30, y = 3.5, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = as.Date("2018-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2018-10-01")+30, y = 3.5, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2019-10-01")+30, y = 3.5, label = "Ages 10-11", angle = 90)+
  geom_vline(xintercept = as.Date("2020-10-01")) + 
  annotate("text", x = as.Date("2020-10-01")+30, y = 3.5, label = "Ages 11-12 & 50-65", angle = 90)+
  geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2021-10-01")+30, y = 3.5, label = "Ages 12-16", angle = 90)  + 
  lims(y = c(0,4))

# look at flu season year. I.e. october to end of march
temp[date_time2 >= as.Date(paste0(year(date_time2), "-10-01")) & 
       date_time2 < as.Date(paste0(year(date_time2)+1, "-04-01")) , flu_year := year(date_time2)]

# drop anything with NA (off season) or 2023 (as dont have full season)
temp <- temp[!is.na(flu_year) & flu_year != 2023,]

# sum across flu year
flu_total <- temp[, sum(V1), by = c("flu_year", "AGE_BAND_NEW")]
# relabel
flu_total[, prev_year := flu_year -1]
flu_total[flu_total, on = c(prev_year = "flu_year", "AGE_BAND_NEW"), prev_year_value := i.V1 ]
flu_total <- flu_total[, c("flu_year", "AGE_BAND_NEW", "V1", "prev_year", "prev_year_value")]
# first year use itself
flu_total[is.na(prev_year_value), prev_year_value := V1]
# proportion change
flu_total[,prop_change := (V1/prev_year_value)-1]

#create plot across all timespan
FLU_CHANGE <- ggplot(flu_total, aes(x = flu_year, y = prop_change, colour = AGE_BAND_NEW)) +
  annotate("rect", xmin = 2019.5, xmax = 2022, ymin = -0.7, ymax = 1.8,
           alpha = 0.4,fill = "grey") +
  geom_line() + theme_bw() + 
  geom_point() + 
  labs(x = "Flu season", y = "Change in number of prescirptions from previous year", 
       title = "A: RTI antibiotics (flu season), excluding Amoxicillin", 
       colour = "Age band") +
  geom_vline(xintercept = 2016-0.4, linetype = "dashed") + 
  annotate("text", x = 2016-0.3, y = 1.5, label = "Ages 7-8", angle = 90)+
  geom_vline(xintercept = 2017-0.4, linetype = "dashed") + 
  annotate("text", x = 2017-0.3, y = 1.5, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = 2018-0.4, linetype = "dashed") + 
  annotate("text", x = 2018-0.3, y = 1.5, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = 2019-0.4, linetype = "dashed") + 
  annotate("text", x = 2019-0.3, y = 1.5, label = "Ages 10-11", angle = 90)+
  geom_vline(xintercept = 2020-0.4) + 
  annotate("text", x = 2020-0.3, y = 1.5, label = "Ages 11-12 & 50-65", angle = 90)+
  geom_vline(xintercept = 2021-0.4,linetype = "dashed") + 
  annotate("text", x = 2021-0.3, y = 1.5, label = "Ages 12-16", angle = 90) + 
  lims(y = c(-0.7,1.8)) + 
  geom_hline(yintercept = 0)

#create plot without covid years
FLU_CHANGE <- ggplot(flu_total[flu_year < 2020, ], aes(x = flu_year, y = prop_change, colour = AGE_BAND_NEW)) +
  # annotate("rect", xmin = 2019.5, xmax = 2019.8, ymin = -0.7, ymax = 1.8,
  #          alpha = 0.4,fill = "grey") +
  geom_line() + theme_bw() + 
  geom_point() + 
  labs(x = "Flu season", y = "Change in number of prescirptions from previous year", 
       title = "B: RTI antibiotics (flu season), excluding Amoxicillin", 
       colour = "Age band") +
  geom_vline(xintercept = 2016-0.4, linetype = "dashed") + 
  annotate("text", x = 2016-0.3, y = 0.2, label = "Ages 7-8", angle = 90)+
  geom_vline(xintercept = 2017-0.4, linetype = "dashed") + 
  annotate("text", x = 2017-0.3, y = 0.2, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = 2018-0.4, linetype = "dashed") + 
  annotate("text", x = 2018-0.3, y = 0.2, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = 2019-0.4, linetype = "dashed") + 
  annotate("text", x = 2019-0.3, y = 0.2, label = "Ages 10-11", angle = 90)+
  # geom_vline(xintercept = 2020-0.4) + 
  # annotate("text", x = 2020-0.3, y = 1.5, label = "Ages 11-12 & 50-65", angle = 90)+
  # geom_vline(xintercept = 2021-0.4,linetype = "dashed") + 
  # annotate("text", x = 2021-0.3, y = 1.5, label = "Ages 12-16", angle = 90) + 
  lims(y = c(-0.3,0.3)) + 
  geom_hline(yintercept = 0)


print(FLU_CHANGE)
