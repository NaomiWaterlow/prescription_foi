# intial data look without the ICBs. 
library(stringr)
library(data.table)
library(ggplot2)


#create lookup
all_data <- fread("data/FOI01975.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]

all_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]
# Split year and month for later use
all_data[, "YEAR" := as.numeric(substr(YEAR_MONTH, 1, 4)),]
all_data[, "MONTH" := as.numeric(substr(YEAR_MONTH, 5, 6)),]

# convert to numeric -> the "*"s get converted to NAs
all_data[, UNIQUE_PATIENT_COUNT := as.numeric(UNIQUE_PATIENT_COUNT)]
# Change stars to -1 so can be used in numeric
all_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
# convert to numeric -> the "*"s get converted to NAs
all_data[, ITEMS := as.numeric(ITEMS)]
# Change stars to -1 so can be used in numeric
all_data[is.na(ITEMS), ITEMS := 1]

fwrite(all_data, "data/all_data.csv")

#drop those in the unknown or interderminate gender / age 
all_data_ex <- all_data[GENDER != "Indeterminate" & GENDER != "Unknown" & AGE_BAND != "Unknown"]




for(i in drugs_lookup$BNF_CHEMICAL_SUBSTANCE_CODE){
  
  target <- i 
#  target <- "0501013B0"
  target_name <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  
  target_data <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  ggplot(target_data,aes(x= AGE_BAND, y = ITEMS, colour = YEAR) ) +
    geom_jitter( alpha =0.5) +
    labs(title = target_name) + facet_grid(.~MONTH)
  
  # # percentage of data rows that are in the 1-4 category
  prop_1_4 <- table(target_data$ITEMS == 1)["TRUE"] / dim(target_data)[1]
  print(paste0(target_name, "--- Proportion of data points with 1-4 items prescribed: ", round(prop_1_4,2)))
  
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, p_1_4 := prop_1_4]
  
  # total number of rows in the original dataset
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_entries := dim(target_data)[1]]
  # total number of prescriptions
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_prescriptions := sum(target_data$ITEMS)]
  
}

# want to look by population. 

# Load in the population sizes


pop_sizes_to_21 <- fread("data/MYEB2_detailed_components_of_change_series_EW_(2020_geog20).csv")
pop_sizes_to_21 <- pop_sizes_to_21[, c("laname20", "age", "sex", "population_2015", "population_2016", 
                        "population_2017", "population_2018", "population_2019", "population_2020")]

pop_sizes_to_21[,pop_2015 :=  sum(population_2015), by = c("age", "sex")]
pop_sizes_to_21[,pop_2016 :=  sum(population_2016), by = c("age", "sex")]
pop_sizes_to_21[,pop_2017 :=  sum(population_2017), by = c("age", "sex")]
pop_sizes_to_21[,pop_2018 :=  sum(population_2018), by = c("age", "sex")]
pop_sizes_to_21[,pop_2019 :=  sum(population_2019), by = c("age", "sex")]
pop_sizes_to_21[,pop_2020 :=  sum(population_2020), by = c("age", "sex")]
pop_sizes_to_21[,c("laname20","population_2015", "population_2016", 
                   "population_2017", "population_2018", "population_2019", "population_2020") := NULL ]
pop_sizes_to_21 <- unique(pop_sizes_to_21)

pop_sizes_to_21[sex == 1, sex2 := "M"]
pop_sizes_to_21[sex == 2, sex2 := "F"]
pop_sizes_to_21[, sex := sex2]
pop_sizes_to_21[, sex2 := NULL]

pop_sizes_to_23 <- fread("data/pop_21_22.csv", header = T)
pop_sizes_to_21[pop_sizes_to_23, on = c("age", "sex"), pop_2021 := i.2021]
pop_sizes_to_21[pop_sizes_to_23, on = c("age", "sex"), pop_2022 := i.2022]
# for now assume 2023 is the same as 2022, as the data is not yet published
pop_sizes_to_21[pop_sizes_to_23, on = c("age", "sex"), pop_2023 := i.2022]


pop_sizes_to_21[age %in% c(0,1), AGE_BAND := "0-1"] 
pop_sizes_to_21[age >= 2 & age <=5, AGE_BAND := "2-5"] 
pop_sizes_to_21[age >= 6 & age <=10, AGE_BAND := "6-10"] 
pop_sizes_to_21[age >= 11 & age <=15, AGE_BAND := "11-15"] 
pop_sizes_to_21[age >= 16 & age <=20, AGE_BAND := "16-20"] 
pop_sizes_to_21[age >= 21 & age <=25, AGE_BAND := "21-25"] 
pop_sizes_to_21[age >= 26 & age <=30, AGE_BAND := "26-30"] 
pop_sizes_to_21[age >= 31 & age <=35, AGE_BAND := "31-35"] 
pop_sizes_to_21[age >= 36 & age <=40, AGE_BAND := "36-40"] 
pop_sizes_to_21[age >= 41 & age <=45, AGE_BAND := "41-45"] 
pop_sizes_to_21[age >= 46 & age <=50, AGE_BAND := "46-50"] 
pop_sizes_to_21[age >= 51 & age <=55, AGE_BAND := "51-55"] 
pop_sizes_to_21[age >= 56 & age <=60, AGE_BAND := "56-60"] 
pop_sizes_to_21[age >= 61 & age <=65, AGE_BAND := "61-65"] 
pop_sizes_to_21[age >= 66 & age <=70, AGE_BAND := "66-70"] 
pop_sizes_to_21[age >= 71 & age <=75, AGE_BAND := "71-75"] 
pop_sizes_to_21[age >= 76 & age <=80, AGE_BAND := "76-80"] 
pop_sizes_to_21[age >= 81 & age <=85, AGE_BAND := "81-85"] 
pop_sizes_to_21[age >= 86 , AGE_BAND := "86+"] 

pop_sizes_to_21[,pop_2015_c :=  sum(pop_2015), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2016_c :=  sum(pop_2016), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2017_c :=  sum(pop_2017), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2018_c :=  sum(pop_2018), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2019_c :=  sum(pop_2019), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2020_c :=  sum(pop_2020), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2021_c :=  sum(pop_2021), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2022_c :=  sum(pop_2022), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,pop_2023_c :=  sum(pop_2023), by = c("AGE_BAND", "sex")]
pop_sizes_to_21[,c("age","pop_2015", "pop_2016", 
                   "pop_2017", "pop_2018", "pop_2019", "pop_2020", "pop_2021", 
                   "pop_2022", "pop_2023") := NULL ]
pop_sizes_to_21 <- unique(pop_sizes_to_21)


# need to format the data into the population based age-bands
all_data_ex[AGE_BAND == "86-90", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "91-95", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "96-100", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "101-105", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "105+", AGE_BAND := "86+" ]

all_data_ex <- all_data_ex[, ITEMS := sum(ITEMS),
                                      by = c("BNF_CHEMICAL_SUBSTANCE_CODE", "AGE_BAND","GENDER" ,"YEAR", "MONTH", "drug_name")]
all_data_ex[,"UNIQUE_PATIENT_COUNT" := NULL]
all_data_ex <- unique(all_data_ex)


pop_sizes_all <- melt.data.table(pop_sizes_to_21, id.vars = c("sex", "AGE_BAND"))
pop_sizes_all[variable == "pop_2015_c", YEAR := 2015]
pop_sizes_all[variable == "pop_2016_c", YEAR := 2016]
pop_sizes_all[variable == "pop_2017_c", YEAR := 2017]
pop_sizes_all[variable == "pop_2018_c", YEAR := 2018]
pop_sizes_all[variable == "pop_2019_c", YEAR := 2019]
pop_sizes_all[variable == "pop_2020_c", YEAR := 2020]
pop_sizes_all[variable == "pop_2021_c", YEAR := 2021]
pop_sizes_all[variable == "pop_2022_c", YEAR := 2022]
pop_sizes_all[variable == "pop_2023_c", YEAR := 2023]
pop_sizes_all[sex == "M", GENDER := "Female"]
pop_sizes_all[sex == "F", GENDER := "Male"]

all_data_ex[pop_sizes_all, on = c("AGE_BAND", "GENDER", "YEAR"), population := i.value]

all_data_ex[, per_100k := (ITEMS/ population)*100000]

all_data_ex[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")]
all_data_ex[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
all_data_ex[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")]




fwrite(all_data_ex, "data/all_data_organised.csv")


all_data_ex[,MONTH := as.factor(MONTH)]
cc <- scales::seq_gradient_pal("blue", "darkorange", "Lab")(seq(0,1,length.out=length(unique(all_data_ex$AGE_BAND))))


for(i in drugs_lookup$BNF_CHEMICAL_SUBSTANCE_CODE){
  
  target <- i 
  #  target <- "0501013B0"
  target_name <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  
  target_data <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  PLOT_TEMP <-  ggplot(target_data,aes(x= date_time2, y = per_100k, colour = AGE_BAND, group = AGE_BAND) ) +
    geom_point( size =0.3) +
    geom_line()+
    labs(title = target_name) + facet_grid(GENDER~.) + 
    theme_bw() + 
    labs(x = "Year", y = "Prescriptions per 100k population") + 
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_color_manual(values = cc)
  print(target_name)
  ggsave(paste0("plots/per_pop/",str_replace_all(target_name, "[^[:alnum:]]", " "),
                "_overview.pdf"), plot = PLOT_TEMP, 
         width = 20, height = 10)

  
}




