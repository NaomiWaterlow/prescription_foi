############ National data exploration ################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################


### Data 
# create lookup
# National data: 288281 rows // April 2015 to December 2023 // * = < 5 records
all_data <- fread("data/FOI01975.csv")
# Antibiotics from "FOI01671_Reference_Tables.xlsx" 
# with additional beta-lactam and respiratory tract infeciton information (RTI)
drugs_lookup <- fread("data/drugs_lookup.csv")
# add in drug name
all_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
# set levels on age band
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
# convert to numeric -> the "*"s get converted to NAs
all_data[, ITEMS := as.numeric(ITEMS)]

if(sensitivity_choice == "default_1"){
  
  all_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
  
  all_data[is.na(ITEMS), ITEMS := 1]
  
} else if (sensitivity_choice == "sens_4"){
  
  all_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 4]
  
  all_data[is.na(ITEMS), ITEMS := 4]
  
} else if (sensitivity_choice == "m4_f1"){
  
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             GENDER == "Male", UNIQUE_PATIENT_COUNT := 4]
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             GENDER == "Female", UNIQUE_PATIENT_COUNT := 1]
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             GENDER == "Indeterminate", UNIQUE_PATIENT_COUNT := 0]
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             GENDER == "Unknown", UNIQUE_PATIENT_COUNT := 0]
  
  all_data[is.na(ITEMS) & 
             GENDER == "Male", ITEMS := 4]
  all_data[is.na(ITEMS) & 
             GENDER == "Female", ITEMS := 1]
  all_data[is.na(ITEMS) & 
             GENDER == "Indeterminate", ITEMS := 0]
  all_data[is.na(ITEMS) & 
             GENDER == "Unknown", ITEMS := 0]
  
} else if (sensitivity_choice == "20_cutoff"){
  
  all_data[is.na(UNIQUE_PATIENT_COUNT), UNIQUE_PATIENT_COUNT := 1]
  
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             AGE_BAND %in% c("0-1", "2-5", "6-10", 
                             "11-15", "16-20"), UNIQUE_PATIENT_COUNT := 4]
  
  all_data[is.na(UNIQUE_PATIENT_COUNT) & 
             AGE_BAND %in% c("Unknown"), UNIQUE_PATIENT_COUNT := 0]
  
  all_data[is.na(ITEMS), ITEMS := 1]
  
  all_data[is.na(ITEMS) & 
             AGE_BAND %in% c("0-1", "2-5", "6-10", 
                             "11-15", "16-20"), ITEMS := 4]
  
  all_data[is.na(ITEMS) & 
             AGE_BAND %in% c("Unknown"), ITEMS := 0]
  
} else {
  print("Not a valid sensitivity choice")
  stop()
}

#### Save re-formatted data
fwrite(all_data, paste0("data/",sensitivity_choice,"/all_data_",sensitivity_choice,".csv"))



# drop all the ones that have less than an average of 10 prescriptions per year (i.e. < 90 prescriptions in total)
temp <- all_data[, sum(ITEMS), by = "drug_name"]
to_remove_low <- (temp[which(temp$V1 < 90)]$drug_name) # Removes 22 drugs

# check the trend in Unknown age bands 

# Add in a date_time standardised column 
all_data[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")] # add a zero to convert single month value to 2digits
all_data[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
all_data[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")] # in standard date time format

# total by date
tot_year_month <- all_data[, sum(ITEMS), by =c("date_time2")]

#percent unknown by age
temp_age <- all_data[, sum(ITEMS), by = c("date_time2", "AGE_BAND")]
temp_age[tot_year_month, on = c("date_time2"), total := i.V1]
temp_age[, percent := (V1/total)*100]
temp_age[, type := "Age Band"]


#percent unknown by gender
temp_gender <- all_data[, sum(ITEMS), by = c("date_time2", "GENDER")]
temp_gender[tot_year_month, on = c("date_time2"), total := i.V1]
temp_gender[, percent := (V1/total)*100]
temp_gender[, type := "Gender"]

# combine for plotting
temp_both <- rbind(temp_gender[GENDER == "Unknown", c("date_time2", "percent", "type")], 
                   temp_age[AGE_BAND == "Unknown", c("date_time2", "percent", "type")])

# PLOT
UNKNOWN_TIME <- ggplot(temp_both, aes(x = date_time2, y = percent, colour = type)) + 
  geom_line()+ 
  labs(x = "Date", y = "Percent Unknown", type = "Variable") + 
  theme_bw()
#save for supplement
ggsave(paste0("plots/",sensitivity_choice,"/unknown_time.pdf"), plot = UNKNOWN_TIME, 
       width = 10, height = 5)


# calculate Unknowns by drug 
tot_drug <- all_data[, sum(ITEMS), by =c("drug_name")]
temp_age_drug <- all_data[, sum(ITEMS), by = c("drug_name", "AGE_BAND")]
temp_age_drug[tot_drug, on = c("drug_name"), total := i.V1]
temp_age_drug[, percent := (V1/total)*100]
temp_age_drug[, type := "Age Band"]
temp_gender_drug <- all_data[, sum(ITEMS), by = c("drug_name", "GENDER")]
temp_gender_drug[tot_drug, on = c("drug_name"), total := i.V1]
temp_gender_drug[, percent := (V1/total)*100]
temp_gender_drug[, type := "Gender"]

# combine for plotting
temp_both_drug <- rbind(temp_gender_drug[GENDER == "Unknown", c("drug_name", "percent", "type", "V1")], 
                        temp_age_drug[AGE_BAND == "Unknown", c("drug_name", "percent", "type", "V1")])

temp_both_drug_m <- melt.data.table(temp_both_drug, id.vars = c("drug_name", "type"))
temp_both_drug_m[variable == "V1", variable := "number"]
temp_both_drug_m[drugs_lookup, on = c(drug_name = "CHEMICAL_SUBSTANCE_BNF_DESCR"), drug_type := i.Drug_type]

## Add in short titles for figure formatting 
temp_both_drug_m[drug_type == "Penicillins", short_title :=  "Penicillins"]
temp_both_drug_m[drug_type == "Macrolides", short_title :=  "Macrolides"]
temp_both_drug_m[drug_type == "Cephalosporins and other beta-lactams", short_title :=  "Ceph's"]
temp_both_drug_m[drug_type == "Quinolones", short_title :=  "Quinolones"]
temp_both_drug_m[drug_type == "Clindamycin and lincomycin", short_title :=  "C&L"]
temp_both_drug_m[drug_type == "Sulfonamides and trimethoprim", short_title :=  "S&T"]
temp_both_drug_m[drug_type == "Some other antibacterials", short_title :=  "Other"]
temp_both_drug_m[drug_type == "Antileprotic drugs", short_title :=  "Lep"]
temp_both_drug_m[drug_type == "Tetracyclines", short_title :=  "Tetracyclines"]
temp_both_drug_m[drug_type == "Antituberculosis drugs", short_title :=  "TB"]
temp_both_drug_m[drug_type == "Urinary-tract infections", short_title :=  "UTIs"]
temp_both_drug_m[drug_type == "Metronidazole, tinidazole and ornidazole", short_title :=  "MTO"]
# Remove those with NA weightings 

UNKNOWN_DRUG <- ggplot(temp_both_drug_m, aes(x = drug_name, y = value, fill = type)) + 
  geom_bar(stat="Identity", position = "dodge") + 
  facet_grid(variable~short_title, scales = "free", space = "free_x") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Antibiotic name")

#save for supplement
ggsave(paste0("plots/",sensitivity_choice,"/unknown_drug.pdf"), plot = UNKNOWN_DRUG, 
       width = 15, height = 10)

# how many Indeterminate gender
Indeterminate <- all_data[, sum(ITEMS), by = "GENDER"]
# NUmber
Indeterminate[GENDER == "Indeterminate", "V1"]
# Percent
(Indeterminate[GENDER == "Indeterminate", "V1"]/ all_data[, sum(ITEMS)])*100


#drop those in the unknown or intederminate gender / age 
all_data_ex <- all_data[GENDER != "Indeterminate" & GENDER != "Unknown" & AGE_BAND != "Unknown" & 
                          !(drug_name %in% to_remove_low)] 

100 * dim(all_data_ex)[1] / dim(all_data)[1] # 74% of the rows kept
100 - 100 * sum(all_data_ex$ITEMS) / sum(all_data$ITEMS) # but only 4% of items removed

# check folder is there for saving the plots into
if(!file.exists(paste0("plots/", sensitivity_choice, "/seasonality"))){
  dir.create(paste0("plots/", sensitivity_choice, "/seasonality"))}
#### Explore seasonality by age and add total number of drugs etc to main data
for(i in unique(all_data_ex$BNF_CHEMICAL_SUBSTANCE_CODE)){
  
  target <- i 
  #target <- "0501013B0"
  target_name <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  
  target_data <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  # Explore monthly and age variation by year 
  TEMP <- ggplot(target_data,aes(x= AGE_BAND, y = ITEMS, colour = YEAR) ) +
    geom_jitter( alpha =0.5) +
    labs(title = target_name,x = "Age Band", y = "Prescription items", 
         colour = "Year") + facet_grid(.~MONTH) + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave(paste0("plots/", sensitivity_choice, "/seasonality/",str_replace_all(target_name, "[^[:alnum:]]", "_"),
                "_seasonality_",sensitivity_choice,".pdf"), plot = TEMP, 
         width = 20, height = 10)
  
  if(sensitivity_choice == "default_1"){
    # # percentage of data rows that are in the 1-4 category
    prop_1_4 <- table(target_data$ITEMS == 1)["TRUE"] / dim(target_data)[1]
    print(paste0(target_name, "--- Proportion of data points with 1-4 items prescribed: ", round(prop_1_4,2)))
    
    drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, p_1_4 := prop_1_4]
    
    # total number of rows in the original dataset
    drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_entries := dim(target_data)[1]]
    
  }
  
  # total number of prescriptions
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_prescriptions := sum(target_data$ITEMS)]
}

############ Need to normalise prescriptions by the number of people in age group ########################

# check for seasonalisty
seasonality_check <- all_data_ex[date_time2<"2020-04-01",]
seasonality_check <- seasonality_check[, sum(ITEMS), by = c("drug_name", "MONTH")]
# calculate average drugs given
seasonality_check[, average_annual := mean(V1), by = c("drug_name")]
# calcultae difference in percent by month
seasonality_check[, percent_dif := ((V1/average_annual)*100) -100]
sc<- seasonality_check[abs(percent_dif) >20]
count_seasonal <- 0
for(drug in unique(sc$drug_name)){
  
  if(length(sc[drug_name == drug,]$MONTH) >= 2){
    count_seasonal <- count_seasonal+1
    print(paste0(drug, " is seasonal"))
  }

}

# percentage of drugs that are seasonal
count_seasonal
count_seasonal/length(unique(seasonality_check$drug_name)) *100


###### Load in the population sizes
## Up to 2021
pop_sizes_to_23 <- fread("data/myebtablesenglandwales20112023.csv", header = T)
pop_sizes_to_23 <- pop_sizes_to_23[, c("laname23","country", "age", "sex", "population_2015", "population_2016", 
                                       "population_2017", "population_2018", "population_2019", "population_2020", 
                                       "population_2021", "population_2022", "population_2023")]
# Only keep England
pop_sizes_to_23 <- pop_sizes_to_23[country == "E",]

pop_sizes_to_23[,pop_2015 :=  sum(population_2015), by = c("age", "sex")]
pop_sizes_to_23[,pop_2016 :=  sum(population_2016), by = c("age", "sex")]
pop_sizes_to_23[,pop_2017 :=  sum(population_2017), by = c("age", "sex")]
pop_sizes_to_23[,pop_2018 :=  sum(population_2018), by = c("age", "sex")]
pop_sizes_to_23[,pop_2019 :=  sum(population_2019), by = c("age", "sex")]
pop_sizes_to_23[,pop_2020 :=  sum(population_2020), by = c("age", "sex")]
pop_sizes_to_23[,pop_2021 :=  sum(population_2021), by = c("age", "sex")]
pop_sizes_to_23[,pop_2022 :=  sum(population_2022), by = c("age", "sex")]
pop_sizes_to_23[,pop_2023 :=  sum(population_2023), by = c("age", "sex")]
pop_sizes_to_23[,c("laname23","population_2015", "population_2016", 
                   "population_2017", "population_2018", "population_2019", "population_2020", 
                   "population_2021", "population_2022", "population_2023") := NULL ]
pop_sizes_to_23 <- unique(pop_sizes_to_23)

# convert sex to match variable in national prescription data 
pop_sizes_to_23[sex == "M", GENDER := "Male"] 
pop_sizes_to_23[sex == "F", GENDER := "Female"] 
pop_sizes_to_23[, sex := NULL]

## Group ages into age groups to match prescription data
pop_sizes_to_23[age %in% c(0,1), AGE_BAND := "0-1"] 
pop_sizes_to_23[age >= 2 & age <=5, AGE_BAND := "2-5"] 
pop_sizes_to_23[age >= 6 & age <=10, AGE_BAND := "6-10"] 
pop_sizes_to_23[age >= 11 & age <=15, AGE_BAND := "11-15"] 
pop_sizes_to_23[age >= 16 & age <=20, AGE_BAND := "16-20"] 
pop_sizes_to_23[age >= 21 & age <=25, AGE_BAND := "21-25"] 
pop_sizes_to_23[age >= 26 & age <=30, AGE_BAND := "26-30"] 
pop_sizes_to_23[age >= 31 & age <=35, AGE_BAND := "31-35"] 
pop_sizes_to_23[age >= 36 & age <=40, AGE_BAND := "36-40"] 
pop_sizes_to_23[age >= 41 & age <=45, AGE_BAND := "41-45"] 
pop_sizes_to_23[age >= 46 & age <=50, AGE_BAND := "46-50"] 
pop_sizes_to_23[age >= 51 & age <=55, AGE_BAND := "51-55"] 
pop_sizes_to_23[age >= 56 & age <=60, AGE_BAND := "56-60"] 
pop_sizes_to_23[age >= 61 & age <=65, AGE_BAND := "61-65"] 
pop_sizes_to_23[age >= 66 & age <=70, AGE_BAND := "66-70"] 
pop_sizes_to_23[age >= 71 & age <=75, AGE_BAND := "71-75"] 
pop_sizes_to_23[age >= 76 & age <=80, AGE_BAND := "76-80"] 
pop_sizes_to_23[age >= 81 & age <=85, AGE_BAND := "81-85"] 
pop_sizes_to_23[age >= 86 , AGE_BAND := "86+"] 

# Sum up population in each age band by sex 
pop_sizes_to_23[,pop_2015_c :=  sum(pop_2015), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2016_c :=  sum(pop_2016), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2017_c :=  sum(pop_2017), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2018_c :=  sum(pop_2018), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2019_c :=  sum(pop_2019), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2020_c :=  sum(pop_2020), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2021_c :=  sum(pop_2021), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2022_c :=  sum(pop_2022), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,pop_2023_c :=  sum(pop_2023), by = c("AGE_BAND", "GENDER")]
pop_sizes_to_23[,c("age","pop_2015", "pop_2016", 
                   "pop_2017", "pop_2018", "pop_2019", "pop_2020", "pop_2021", 
                   "pop_2022", "pop_2023", "country") := NULL ]
pop_sizes_to_23 <- unique(pop_sizes_to_23)


# need to format the prescription data into the population based age-bands
# population based estimates only up to 90yo 
all_data_ex[AGE_BAND == "86-90", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "91-95", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "96-100", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "101-105", AGE_BAND := "86+" ]
all_data_ex[AGE_BAND == "105+", AGE_BAND := "86+" ]

# Sum to get number in each by the age groupings and sex
all_data_ex <- all_data_ex[, sum(ITEMS),
                           by = c("BNF_CHEMICAL_SUBSTANCE_CODE", "AGE_BAND","GENDER" ,"YEAR", "MONTH", "drug_name")]
colnames(all_data_ex)[which(colnames(all_data_ex) == "V1")] <- "ITEMS"

# also do this across all drugs
data_overall <- all_data_ex[,sum(ITEMS), by = c("AGE_BAND","GENDER" ,"YEAR", "MONTH" )]
colnames(data_overall)[which(colnames(data_overall) == "V1")] <- "ITEMS"

# Melt population data to merge with prescription data 
pop_sizes_all <- melt.data.table(pop_sizes_to_23, id.vars = c("GENDER", "AGE_BAND"))
pop_sizes_all[variable == "pop_2015_c", YEAR := 2015]
pop_sizes_all[variable == "pop_2016_c", YEAR := 2016]
pop_sizes_all[variable == "pop_2017_c", YEAR := 2017]
pop_sizes_all[variable == "pop_2018_c", YEAR := 2018]
pop_sizes_all[variable == "pop_2019_c", YEAR := 2019]
pop_sizes_all[variable == "pop_2020_c", YEAR := 2020]
pop_sizes_all[variable == "pop_2021_c", YEAR := 2021]
pop_sizes_all[variable == "pop_2022_c", YEAR := 2022]
pop_sizes_all[variable == "pop_2023_c", YEAR := 2023]

# Merge population and prescription data 
all_data_ex[pop_sizes_all, on = c("AGE_BAND", "GENDER", "YEAR"), population := i.value]
data_overall[pop_sizes_all, on = c("AGE_BAND", "GENDER", "YEAR"), population := i.value]
## Checks on population 
# all_data_ex %>% filter(YEAR == 2023, MONTH == 1, drug_name == "Nitrofurantoin") %>%#, #AGE_BAND %in% c("0-1","2-5")) %>%
#   summarise(sum(population)) ## 58 million

# Calculate the per 100,000 population prescription rate 
all_data_ex[, per_100k := (ITEMS/ population)*100000]
data_overall[, per_100k := (ITEMS/ population)*100000]
# Add in a date_time standardised column 
all_data_ex[MONTH <10, date_time := paste0(YEAR, "-0",MONTH,"-01")] # add a zero to convert single month value to 2digits
all_data_ex[MONTH >= 10, date_time := paste0(YEAR, "-",MONTH,"-01")]
all_data_ex[, date_time2 := as.Date(date_time, try.format = "%z-%m-%d")] # in standard date time format
data_overall[all_data_ex, on = c("YEAR", "MONTH"), date_time2 := date_time2]

########### Save organised and reformatted prescription data and population data
fwrite(all_data_ex, paste0("data/",sensitivity_choice,"/all_data_organised_",sensitivity_choice,".csv"))
fwrite(pop_sizes_all, "data/pop_sizes.csv")

#######################################################################
########## Output key figures for all drugs AND FIGURE 1 ##############
#######################################################################
# Need Month as a factor
all_data_ex[,MONTH := as.factor(MONTH)]
# Colour scale by age_band
cc <- scales::seq_gradient_pal("blue", "darkorange", "Lab")(seq(0,1,length.out=length(unique(all_data_ex$AGE_BAND))))

# Generate plots in per_pop folder 
if(!file.exists(paste0("plots/",sensitivity_choice,"/per_pop"))){
  dir.create(file.path(paste0("plots/",sensitivity_choice,"/per_pop")))}

# For each drug generate plots of per population 
for(i in drugs_lookup$BNF_CHEMICAL_SUBSTANCE_CODE){
  
  target <- i 
  # target <- "0501013B0" # Amox # key examples
  # target <- "0501120P0" # Ofloxacin # key examples
  target_name <- drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target]$CHEMICAL_SUBSTANCE_BNF_DESCR[1]
  
  target_data <- all_data_ex[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  if(nrow(target_data) > 0 ){
    PLOT_TEMP <-  ggplot(target_data,aes(x= date_time2, y = per_100k, colour = AGE_BAND, group = AGE_BAND) ) +
      annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-07-01"), ymin = 0, ymax = max(target_data$per_100k),
               alpha = 0.4,fill = "grey") +
      geom_point( size =0.3) +
      geom_line()+
      labs(title = target_name) + facet_grid(GENDER~.) + 
      theme_bw() + 
      labs(x = "Month", y = "Prescriptions per 100k population", colour = "Age band") + 
      guides(color = guide_legend(override.aes = list(size=2))) +
      #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_color_manual(values = cc)
    
    # Save these for Figure 1 plot below
    if(target == "0501050B0"){CLARITH_PLOT <- PLOT_TEMP + labs(title= "A: Clarithromycin") }
    if(target == "0501120P0"){OF_PLOT <- PLOT_TEMP+ labs(title= "B: Ofloxacin")  }
    
    print(target_name)
    

    
    ggsave(paste0("plots/",sensitivity_choice,"/per_pop/",str_replace_all(target_name, "[^[:alnum:]]", "_"),
                  "_overview_",sensitivity_choice,".pdf"), plot = PLOT_TEMP+ theme(title = element_text(size =25), 
                                                                                   axis.title = element_text(size =20), 
                                                                                   axis.text = element_text(size =20), 
                                                                                   legend.text = element_text(size =20), 
                                                                                   legend.title = element_text(size =20)), 
           width = 20, height = 10)
    
  }
}

#generate overall drugs plot
PLOT_OVERALL <-  ggplot(data_overall,aes(x= date_time2, y = per_100k, colour = AGE_BAND, group = AGE_BAND) ) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-07-01"), ymin = 0, ymax = max(data_overall$per_100k),
         alpha = 0.4,fill = "grey") +
  geom_point( size =0.3) +
  geom_line()+
  labs(title = "All prescriptions") + facet_grid(GENDER~.) + 
  theme_bw() + 
  labs(x = "Month", y = "Prescriptions per 100k population", colour = "Age band") + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  scale_color_manual(values = cc)


ggsave(paste0("plots/",sensitivity_choice,"/per_pop/all_perscriptions_",sensitivity_choice,".pdf"),
       plot = PLOT_OVERALL, 
       width = 20, height = 10)


###### Heat map of male vs female rates
# Exclude those with small numbers to guarantee sufficient prescriptions
# Use only those with more than 100k over whole time period
drugs_to_exclude <- drugs_lookup[total_prescriptions<100000,]$CHEMICAL_SUBSTANCE_BNF_DESCR
# removes 63 drugs

# average per year
all_data_ex[drugs_lookup, on=c(drug_name = "CHEMICAL_SUBSTANCE_BNF_DESCR"), drug_type := i.Drug_type]
all_data_annual <- all_data_ex[!(drug_name %in% drugs_to_exclude), mean(per_100k), by = c("drug_name", "GENDER", "AGE_BAND", "YEAR", "drug_type")]

# Just use data from latest year
target_year <- 2023

# cast to get male and female on same row
relative_weightings <- dcast.data.table(all_data_annual, drug_name + AGE_BAND + YEAR + drug_type ~ GENDER, value.var = "V1")
# take log of the relative levels 
relative_weightings[, relative_gender := log(Female/Male)]

# factor drug name
relative_weightings$drug_name <- factor(relative_weightings$drug_name)

## Add in short titles for figure formatting 
relative_weightings[drug_type == "Penicillins", short_title :=  "Penicillins"]
relative_weightings[drug_type == "Macrolides", short_title :=  "Macrolides"]
relative_weightings[drug_type == "Cephalosporins and other beta-lactams", short_title :=  "Ceph's"]
relative_weightings[drug_type == "Quinolones", short_title :=  "Quinolones"]
relative_weightings[drug_type == "Clindamycin and lincomycin", short_title :=  "C&L"]
relative_weightings[drug_type == "Sulfonamides and trimethoprim", short_title :=  "S&T"]
relative_weightings[drug_type == "Some other antibacterials", short_title :=  "Other"]
relative_weightings[drug_type == "Antileprotic drugs", short_title :=  "Lep"]
relative_weightings[drug_type == "Tetracyclines", short_title :=  "Tetracyclines"]
relative_weightings[drug_type == "Antituberculosis drugs", short_title :=  "TB"]
relative_weightings[drug_type == "Urinary-tract infections", short_title :=  "UTIs"]
relative_weightings[drug_type == "Metronidazole, tinidazole and ornidazole", short_title :=  "MTO"]
# Remove those with NA weightings 
relative_weightings <- relative_weightings[!is.na(relative_gender),]
# removes 99 rows = ~ 2%

### Heat map of relative levels by gender 
RELATIVE_GENDER <- ggplot(relative_weightings[YEAR == target_year], aes(x = AGE_BAND, y = drug_name, fill = relative_gender)) + 
  geom_tile() + 
  theme_linedraw() +
  scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" ) + 
  facet_grid(short_title~., scales = "free_y", space = "free") + 
  labs(x = "Age Band", y= "Antibiotic", fill = "relative rate (log)", 
       title = paste0("C: Relative prescription rate by sex (annual prescriptions for ",target_year,")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
RELATIVE_GENDER
ggsave(paste0("plots/",sensitivity_choice,"/per_pop/relative_gender_",target_year,".pdf"), 
       plot = RELATIVE_GENDER, width = 20, height = 10)

LEG <- get_legend(CLARITH_PLOT) 

FIG1 <- grid.arrange(CLARITH_PLOT + theme(legend.position = "NONE"), OF_PLOT+ theme(legend.position = "NONE"), LEG ,RELATIVE_GENDER, layout_matrix = rbind(c(1,1,1,1,1, 3, 4,4,4,4,4,4,4),
                                                                                                                                                           c(2,2,2,2,2, 3, 4,4,4,4,4,4,4))
)

quantify_gender <- relative_weightings[YEAR == target_year]
quantify_gender[relative_gender < 0, female_higher := 0]
quantify_gender[relative_gender > 0, female_higher := 1]
quantify_gender2 <- quantify_gender[, sum(female_higher), by = "drug_name"]
quantify_gender2$total <-  quantify_gender[, .N, by = "drug_name"]$N
quantify_gender2[V1/total > 0.5, threshold_0.5 := T]
quantify_gender2[V1/total > 0.75, threshold_0.75 := T]
quantify_gender2[V1/total > 0.9, threshold_0.9 := T]

# X% of drugs have over 50% of age groups that prescribe more to female than male
sum(quantify_gender2$threshold_0.5, na.rm = T)/nrow(quantify_gender2)
# X% of drugs have over 75% of age groups that prescribe more to female than male
sum(quantify_gender2$threshold_0.75, na.rm = T)/nrow(quantify_gender2)
# X% of drugs have over 90% of age groups that prescribe more to female than male
sum(quantify_gender2$threshold_0.9, na.rm = T)/nrow(quantify_gender2)

##### Save Figure 1
ggsave(paste0("plots/",sensitivity_choice,"/Fig1_",sensitivity_choice,".pdf"), plot = FIG1, 
       width = 20, height = 10)

################## Table 1 #########################
all_data_ex <- fread(paste0("data/",sensitivity_choice,"/all_data_organised_",sensitivity_choice,".csv"))

# Summary values for results section 
# Total 
format(all_data_ex %>% summarise(sum(ITEMS)), big.mark = ",")
# Per person 
total_pops = all_data_ex %>% group_by(YEAR) %>% filter(MONTH == 12, drug_name == "Nitrofurantoin") %>% 
  summarise(totalp = sum(population))

per_persond <- all_data_ex %>% group_by(YEAR) %>% summarise(total_items = sum(ITEMS)) %>%
  left_join(total_pops) %>% mutate(per_person = total_items / totalp)

paste0(round(mean(per_persond$per_person),2), "(",
       round(min(per_persond$per_person),2),", ",
       round(max(per_persond$per_person),2), ")")


# In women 
inwomend <- all_data_ex %>% group_by(GENDER, YEAR) %>% summarise(total_items = sum(ITEMS)) %>%
  pivot_wider(names_from = GENDER, values_from = total_items) %>%
  mutate(perc_f = 100*Female / (Male + Female))

paste0(round(mean(inwomend$perc_f),0), "(",
       round(min(inwomend$perc_f),0),", ",
       round(max(inwomend$perc_f),0), ")")

# By Gender: amount per person per year 
total_popsg = all_data_ex %>% group_by(GENDER, YEAR) %>% filter(MONTH == 12, drug_name == "Nitrofurantoin") %>% 
  summarise(totalp = sum(population))

per_persondg <- all_data_ex %>% group_by(GENDER, YEAR) %>% summarise(total_items = sum(ITEMS)) %>%
  left_join(total_popsg) %>% mutate(per_person = total_items / totalp) %>%
  select(GENDER, per_person, YEAR) %>%
  pivot_wider(names_from = GENDER, values_from = per_person)

paste0(round(mean(per_persondg$Female),2), "(",
       round(min(per_persondg$Female),2),", ",
       round(max(per_persondg$Female),2), ")")

paste0(round(mean(per_persondg$Male),2), "(",
       round(min(per_persondg$Male),2),", ",
       round(max(per_persondg$Male),2), ")")

# By Age 

aged <- all_data_ex %>% group_by(AGE_BAND, YEAR) %>% 
  summarise(totalp = sum(ITEMS)) %>% pivot_wider(names_from = AGE_BAND,
                                                 values_from = totalp) %>%
  rowwise() %>% 
  mutate(under_15 = sum(`0-1`,`2-5`,`6-10`,`11-15`),
         over_65 = sum(`66-70`, `71-75`, `76-80`, `81-85`, `86+`),
         total = sum(`0-1`, `11-15`, `16-20`, `2-5`, `21-25`, `26-30`, `31-35`, 
                     `36-40`, `41-45`, `46-50`, `51-55`, `56-60`, `6-10`, `61-65`, 
                     `66-70`, `71-75`, `76-80`, `81-85`, `86+`),
         perc_under_15 = 100*under_15/total,
         perc_over_65 = 100*over_65/total) %>%
  select(under_15, over_65, total, perc_under_15, perc_over_65)

paste0(round(mean(aged$perc_under_15),0), "%(",
       round(min(aged$perc_under_15),0),"%, ",
       round(max(aged$perc_under_15),0), "%)")

paste0(round(mean(aged$perc_over_65),0), "%(",
       round(min(aged$perc_over_65),0),"%, ",
       round(max(aged$perc_over_65),0), "%)")



# Table 1 summary values
total_p <- all_data_ex %>% group_by(drug_name) %>% summarise(total_prescriptions = sum(ITEMS)) 
aver_p <- all_data_ex %>% group_by(drug_name, YEAR) %>% summarise(total_per_year = sum(ITEMS)) %>%
  group_by(drug_name) %>% summarise(mean_annual_prescriptions = round(mean(total_per_year),0))
top_month <- all_data_ex %>% group_by(drug_name, MONTH) %>% summarise(total_per_month = sum(ITEMS)) %>%
  group_by(drug_name) %>% mutate(max_monthly = max(total_per_month),
                                 max_month = ifelse(total_per_month == max_monthly,MONTH,"")) %>%
  filter(!max_month == "") %>%
  select(drug_name,max_month) %>%
  group_by(drug_name) %>%
  summarise(top_month = paste(max_month, collapse = ",")) 

perc_female <- all_data_ex %>% group_by(drug_name, GENDER) %>% 
  summarise(total_prescriptions = sum(ITEMS)) %>%
  pivot_wider(names_from = GENDER, values_from = total_prescriptions) %>%
  mutate(perc_female = round(100 * Female/(Female + Male),0)) %>% select(perc_female)

top_age_female_2023 <- all_data_ex %>% filter(YEAR == 2023, GENDER == "Female") %>% 
  group_by(drug_name, AGE_BAND) %>% 
  summarise(total_pop = sum(population),# have to sum over years
            total_ITEMS = sum(ITEMS), 
            per_100k = 100000 * total_ITEMS / total_pop) %>% 
  arrange(desc(per_100k)) %>% slice(1:3) %>% #filter(drug_name == "Amikacin")
  group_by(drug_name) %>%
  arrange(AGE_BAND, .by_group = TRUE) %>% 
  summarise(top_agesf = paste(AGE_BAND, collapse = ",")) 

top_age_male_2023 <- all_data_ex %>% filter(YEAR == 2023, GENDER == "Male") %>% 
  group_by(drug_name, AGE_BAND) %>% 
  summarise(total_pop = sum(population),# have to sum over years
            total_ITEMS = sum(ITEMS), 
            per_100k = 100000 * total_ITEMS / total_pop) %>% 
  arrange(desc(per_100k)) %>% slice(1:3) %>%
  group_by(drug_name) %>%
  arrange(AGE_BAND, .by_group = TRUE) %>% 
  summarise(top_agesm = paste(AGE_BAND, collapse = ",")) 

top_three_years <- all_data_ex %>% filter(YEAR %in% c(2016, 2019, 2023)) %>% 
  group_by(YEAR, drug_name) %>% summarise(total_prescriptions = sum(ITEMS)) %>%
  pivot_wider(names_from = YEAR, values_from = total_prescriptions)

table1_unformatted <- total_p %>% left_join(aver_p) %>% left_join(perc_female)  %>% left_join(top_month) %>% left_join(top_three_years) %>%
  left_join(top_age_female_2023) %>% left_join(top_age_male_2023) %>%
  ungroup() %>% 
  arrange(desc(total_prescriptions))

## Format for output
table1 <- table1_unformatted
table1$total_prescriptions <- format(table1$total_prescriptions, big.mark = ",")
table1$mean_annual_prescriptions <- format(table1$mean_annual_prescriptions, big.mark = ",")
table1$`2016` <- format(table1$`2016`, big.mark = ",")
table1$`2019` <- format(table1$`2019`, big.mark = ",")
table1$`2023` <- format(table1$`2023`, big.mark = ",")

colnames(table1) <- c("Antibiotic", "Total prescriptions", "Mean annual prescriptions",
                      "% to females", 
                      "Month","2016", "2019","2023",
                      "Females","Males")

write.csv(table1, paste0("tables/",sensitivity_choice,"_table1.csv"))

### common month
table(table1[1:20,"Month"])
table(table1[1:20,"% to females"])
length(which(table1_unformatted[1:20,"2023"] - table1_unformatted[1:20,"2016"]<0))
          
      