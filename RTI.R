# does flu vaccine effect prescriptions for RTIs?

all_ICB_data <- fread("data/ICB_Data/all_ICB_data.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", RTI := RTI]


all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]


data_for_RTI<- all_ICB_data[, sum(ITEMS),
                             by = c("YEAR", "AGE_BAND", "MONTH", "drug_name", "BNF_CHEMICAL_SUBSTANCE_CODE", "RTI")]

data_for_RTI <- data_for_RTI[RTI == "Y"]


data_for_RTI[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")]
data_for_RTI[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
data_for_RTI[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")]

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

ggplot(temp, aes(x = date_time2, y = V1, colour = AGE_BAND_NEW)) + 
  geom_line() + theme_bw() + 
  labs(x = "Date", y = "Prescriptions") + 
  geom_vline(xintercept = as.Date("2015-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2015-10-01")+30, y = 600000, label = "Ages 5-7", angle = 90) +
geom_vline(xintercept = as.Date("2016-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2016-10-01")+30, y = 600000, label = "Ages 7-8", angle = 90)+
geom_vline(xintercept = as.Date("2017-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2017-10-01")+30, y = 600000, label = "Ages 4-5 & 8-9", angle = 90) +
  geom_vline(xintercept = as.Date("2018-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2018-10-01")+30, y = 600000, label = "Ages 9-10", angle = 90) + 
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dashed") + 
  annotate("text", x = as.Date("2019-10-01")+30, y = 600000, label = "Ages 10-11", angle = 90)






