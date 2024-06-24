# does flu vaccine effect prescriptions for RTIs?

all_data_ex <- fread("data/all_data_organised.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
all_data_ex[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", RTI := RTI]


all_data_ex[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]

# sum across genders as not interested. 
data_for_RTI<- all_data_ex[, sum(ITEMS),
                             by = c("date_time2", "AGE_BAND", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE", "RTI")]
temp_pops<- all_data_ex[, sum(population),
                           by = c("date_time2", "AGE_BAND", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE", "RTI")]
data_for_RTI[temp_pops, on = c("date_time2", "AGE_BAND", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE", "RTI"), population := i.V1]

# keep only respiratory related drugs
data_for_RTI <- data_for_RTI[RTI == "Y"]

# reformat to relevant age bands
data_for_RTI[, AGE_BAND_NEW := "15-50"]
data_for_RTI[AGE_BAND == "Unknown" , AGE_BAND_NEW := NA]
data_for_RTI[AGE_BAND == "0-1" , AGE_BAND_NEW := "0-1"]
data_for_RTI[AGE_BAND == "2-5" , AGE_BAND_NEW := "2-5"]
data_for_RTI[AGE_BAND == "6-10" , AGE_BAND_NEW := "6-10"]
data_for_RTI[AGE_BAND == "11-15" , AGE_BAND_NEW := "11-15"]
data_for_RTI[AGE_BAND == "51-55" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND == "56-60" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND == "61-65" , AGE_BAND_NEW := "50-65"]
data_for_RTI[AGE_BAND %in% c("66-70", "71-75", "76-80",  "81-85" ,
             "86-90", "91-95","96-100","101-105", "105+"), AGE_BAND_NEW := "65+"]

temp <- data_for_RTI[, sum(V1), by = c("date_time2", "AGE_BAND_NEW", "RTI")]
temp_pop2 <- data_for_RTI[, sum(population), by = c("date_time2", "AGE_BAND_NEW", "RTI")]
temp[temp_pop2, on = c("date_time2", "AGE_BAND_NEW", "RTI"), population := i.V1]
temp[,AGE_BAND_NEW := factor(AGE_BAND_NEW, levels = c("0-1"  , "2-5"  , "6-10",  "11-15", "15-50", "50-65" ,"65+"  ))]

temp[, prescriptions_p_100k := (V1/population)*100000]


ggplot(temp, aes(x = date_time2, y = prescriptions_p_100k, colour = AGE_BAND_NEW)) + 
  geom_line() + theme_bw() + 
  labs(x = "Date", y = "Prescription rate of RTI antibiotics", 
       title = "Vertical lines indicate when flu vaccination in that age group was implemented", 
       colour = "Age band") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2015-10-01")+30, y = 1100, label = "Ages 5-7", angle = 90) +
geom_vline(xintercept = as.Date("2016-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2016-10-01")+30, y = 1100, label = "Ages 7-8", angle = 90)+
geom_vline(xintercept = as.Date("2017-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2017-10-01")+30, y = 1100, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = as.Date("2018-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2018-10-01")+30, y = 1100, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2019-10-01")+30, y = 1100, label = "Ages 10-11", angle = 90)+
geom_vline(xintercept = as.Date("2020-10-01")) + 
  annotate("text", x = as.Date("2020-10-01")+30, y = 1100, label = "Ages 11-12 & 50-65", angle = 90)+
geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2021-10-01")+30, y = 1100, label = "Ages 12-16", angle = 90) 

# normalise compared to 15-50 age group at each time step
temp2 <- temp[AGE_BAND_NEW=="15-50"]
temp[temp2, on = c("date_time2"), mid_age := i.prescriptions_p_100k]

temp[, normalised_rate := prescriptions_p_100k/mid_age]

ggplot(temp, aes(x = date_time2, y = normalised_rate, colour = AGE_BAND_NEW)) + 
  geom_line() + theme_bw() + 
  labs(x = "Date", y = "Relative prescirption rate of RTI antibiotics, cf 15-50 age group", 
       title = "Vertical lines indicate when flu vaccination in that age group was implemented", 
       colour = "Age band") +
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2015-10-01")+30, y = 7, label = "Ages 5-7", angle = 90) +
  geom_vline(xintercept = as.Date("2016-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2016-10-01")+30, y = 7, label = "Ages 7-8", angle = 90)+
  geom_vline(xintercept = as.Date("2017-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2017-10-01")+30, y = 7, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = as.Date("2018-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2018-10-01")+30, y = 7, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2019-10-01")+30, y = 7, label = "Ages 10-11", angle = 90)+
  geom_vline(xintercept = as.Date("2020-10-01")) + 
  annotate("text", x = as.Date("2020-10-01")+30, y = 7, label = "Ages 11-12 & 50-65", angle = 90)+
  geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2021-10-01")+30, y = 7, label = "Ages 12-16", angle = 90) 

