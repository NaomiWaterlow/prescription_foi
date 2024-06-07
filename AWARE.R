# label by AWARE classification
library(data.table)
library(ggplot2)
ICB_data_2023 <- fread("data/2023_per_population.csv")
aware_lookup <- fread("data/AWARE_Classification.csv")

# when name matches add the aware calssification
ICB_data_2023[aware_lookup, on = c("drug_name"), aware_class := i.Category]

# which ones don't match names? 
missing <- unique(ICB_data_2023[is.na(aware_class)]$drug_name)

missing[1]
temp <- aware_lookup$drug_name[grepl("Benzylpenicillin", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[1]]

missing[2]
temp <- aware_lookup$drug_name[grepl("Phenoxymethylpenicillin", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[2]]

missing[3]
temp <- aware_lookup$drug_name[grepl("Flucloxacillin", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[3]]

missing[4]
temp <- aware_lookup$drug_name[grepl("clavulanic", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[4]]

missing[5]
temp <- aware_lookup$drug_name[grepl("Pivmecillinam", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[5]]

missing[6]
temp <- aware_lookup$drug_name[grepl("Cefuroxime", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[6]]

missing[7]
temp <- aware_lookup$drug_name[grepl("Demeclocycline", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[7]]

missing[8]
temp <- aware_lookup$drug_name[grepl("Doxycycline", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[8]]

missing[9]
temp <- aware_lookup$drug_name[grepl("Minocycline", aware_lookup$drug_name, fixed = TRUE)]
### there are two. hydroclorine seems to be the oral version. 
aware_lookup[drug_name == temp[2], matching_name := missing[9]]

missing[10]
temp <- aware_lookup$drug_name[grepl("Doxycycline", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name == temp, matching_name := missing[10]]


missing[11]
temp <- aware_lookup$drug_name[grepl("Gentamicin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[11]]

missing[12]
temp <- aware_lookup$drug_name[grepl("Erythromycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[12]]

missing[13]
temp <- aware_lookup$drug_name[grepl("Erythromycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[13]]

missing[14]
temp <- aware_lookup$drug_name[grepl("Clindamycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[14]]

missing[15]
temp <- aware_lookup$drug_name[grepl("Fosfomycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name%in%temp,]
# there are two/ Trometamol seems to be the oral version
aware_lookup[drug_name == temp[2], matching_name := missing[15]]


missing[16]
temp <- aware_lookup$drug_name[grepl("colistimethate", aware_lookup$drug_name, fixed = TRUE)]
temp # not present. 
aware_lookup[Class == "Polymyxins"] # <- HERE! Oral and IV same classification
aware_lookup[drug_name == "Colistin_oral", matching_name := missing[16]]
 

missing[17]
temp <- aware_lookup$drug_name[grepl("Fusidic", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[17]]

missing[18]
temp <- aware_lookup$drug_name[grepl("Vancomycin", aware_lookup$drug_name, fixed = TRUE)]
temp # 2 but same classification
aware_lookup[drug_name == temp[2], matching_name := missing[18]]

missing[19]
temp <- aware_lookup$drug_name[grepl("Sulfamethoxazole", aware_lookup$drug_name, fixed = TRUE)]
temp# second one
aware_lookup[drug_name == temp[2], matching_name := missing[19]]

missing[22]
temp <- aware_lookup$drug_name[grepl("Metronidazole", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[grepl("Metronidazole", aware_lookup$drug_name, fixed = TRUE),]
#both access
aware_lookup[drug_name == temp[2], matching_name := missing[22]]

missing[24]
temp <- aware_lookup$drug_name[grepl("Ceftriaxone", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[24]]

missing[26]
temp <- aware_lookup$drug_name[grepl("Rifampicin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[26]]

missing[27]
temp <- aware_lookup$drug_name[grepl("Fusidic", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[27]]

missing[28]
temp <- aware_lookup$drug_name[grepl("Cefuroxime", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[28]]



missing[32]
temp <- aware_lookup$drug_name[grepl("Piperacillin", aware_lookup$drug_name, fixed = TRUE)]
temp # second one
aware_lookup[drug_name == temp[2], matching_name := missing[32]]

missing[33]
temp <- aware_lookup$drug_name[grepl("Ceftazidime", aware_lookup$drug_name, fixed = TRUE)]
temp 
aware_lookup[grepl("Ceftazidime", aware_lookup$drug_name, fixed = TRUE),]
# first one
aware_lookup[drug_name == temp[1], matching_name := missing[33]]

missing[34]
temp <- aware_lookup$drug_name[grepl("Amoxicillin", aware_lookup$drug_name, fixed = TRUE)]
temp# same
aware_lookup[drug_name == temp[1], matching_name := missing[34]]

missing[35]
temp <- aware_lookup$drug_name[grepl("Ertapenem", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[35]]

missing[36]
temp <- aware_lookup$drug_name[grepl("Cefotaxime", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[36]]

missing[37]
temp <- aware_lookup$drug_name[grepl("Amoxicillin", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J01XD02", aware_lookup$`ATC code`, fixed = TRUE)]
aware_lookup[`ATC code` == "J01XD02"]
temp <- "Tinidazole_IV"
aware_lookup[drug_name == temp, matching_name := missing[37]]

missing[38]
temp <- aware_lookup$drug_name[grepl("Flucloxacillin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[38]]

missing[39]
temp <- aware_lookup$drug_name[grepl("Ampicillin", aware_lookup$drug_name, fixed = TRUE)]
temp # second one
aware_lookup[drug_name == temp[2], matching_name := missing[39]]

missing[41]
temp <- aware_lookup$drug_name[grepl("Temocillin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[41]]

missing[42]
temp <- aware_lookup$drug_name[grepl("Clindamycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name == temp, matching_name := missing[42]]

missing[43]
temp <- aware_lookup$drug_name[grepl("Neomycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name %in%temp, ]#same
aware_lookup[drug_name == temp[1], matching_name := missing[43]]





####### NOT FOUND ######

# NOT FOUND
missing[29]
temp <- aware_lookup$drug_name[grepl("saly", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J04AA01", aware_lookup$`ATC code`, fixed = TRUE)]
temp

# NOT FOUND  
missing[25]
temp <- aware_lookup$drug_name[grepl("Ethambutol", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J04AK02", aware_lookup$`ATC code`, fixed = TRUE)]
temp

# NOT FOUND
missing[23]
temp <- aware_lookup$drug_name[grepl("Hexamine", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J01XX05", aware_lookup$`ATC code`, fixed = TRUE)]
temp

# NOT FOUND
missing[20]
temp <- aware_lookup$drug_name[grepl("Isonicotinyl", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("JO4AM08", aware_lookup$`ATC code`, fixed = TRUE)]
temp

# NOT FOUND
missing[21]
temp <- aware_lookup$drug_name[grepl("Diaminodiphenyl", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J04BA02", aware_lookup$`ATC code`, fixed = TRUE)]
temp
aware_lookup[grepl("Sulfonamides", aware_lookup$Class, fixed = TRUE),]

# NOT FOUND
missing[30]
temp <- aware_lookup$drug_name[grepl("Pyrazinamide", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J04AK01", aware_lookup$`ATC code`, fixed = TRUE)]
temp

# NOT FOUND
missing[31]
temp <- aware_lookup$drug_name[grepl("Clofazimine", aware_lookup$drug_name, fixed = TRUE)]
temp <- aware_lookup$`ATC code`[grepl("J04BA01", aware_lookup$`ATC code`, fixed = TRUE)]
 temp
# Not sure 

# nots ure 
missing[40]
temp <- aware_lookup$drug_name[grepl("Fosfomycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name %in% temp]


#### ADD the manual ones

aware_lookup_new <- aware_lookup[!(is.na(matching_name))]
aware_lookup_new[,drug_name := NULL]
colnames(aware_lookup_new)[5] <- "drug_name"
# when name matches add the aware calssification
ICB_data_2023[aware_lookup_new, on = c("drug_name"), aware_class := i.Category]

ICB_data_2023_aware <- ICB_data_2023[!is.na(aware_class)]
ICB_data_2023_aware <- dcast.data.table(ICB_data_2023_aware, ICB_CODE + GENDER + YEAR + MONTH +
                           AGE_BAND_NEW + POP + aware_class ~. , value.var = "ITEMS",
                         fun.aggregate = sum)


colnames(ICB_data_2023_aware)[which(colnames(ICB_data_2023_aware) == ".")] <- "ITEMS"
ICB_data_2023_aware[,rate_per_1000 := (ITEMS/POP)*1000]

# average acrosss ICB and Months
ICB_data_2023_aware[, av_across_ICBs_months := mean(rate_per_1000),by = c( "GENDER", "AGE_BAND_NEW", "aware_class")]
ICB_data_2023_aware_av <- unique(ICB_data_2023_aware[,c("ICB_CODE", "MONTH","ITEMS", "rate_per_1000", "POP") := NULL])


ICB_data_2023_aware_av$aware_class <- factor(ICB_data_2023_aware_av$aware_class, 
                                             levels = c("Access", "Watch", "Reserve"))
ICB_data_2023_aware_av[, AGE_BAND_NEW := factor(AGE_BAND_NEW, levels = c("0-1", "2-5", 
                                                                         "6-10", "11-15", "16-20", "21-25", 
                                                                         "26-30", "31-35", "36-40", "41-45", 
                                                                         "46-50", "51-55",  "56-60", "61-65", 
                                                                         "66-70", "71-75", "76-80",  "81-85" ,
                                                                         "86+") )]


ggplot(ICB_data_2023_aware_av, aes(x = AGE_BAND_NEW, y = av_across_ICBs_months, colour = aware_class, group = aware_class)) + 
  geom_line() + 
  facet_grid(GENDER~.) + 
  theme_bw() + 
  labs(y = "average number of prescriptions (across ICBs and months in 2023) per 1000 population",
       title = "AWARE CLASSIFICATION")


tot_rates <- ICB_data_2023_aware_av[, sum(av_across_ICBs_months), by = c("GENDER", "AGE_BAND_NEW")]

ICB_data_2023_aware_av[tot_rates, on=c("GENDER", "AGE_BAND_NEW"), total_rate := i.V1]
ICB_data_2023_aware_av[, perc_each_class := (av_across_ICBs_months/total_rate)*100]


ggplot(ICB_data_2023_aware_av, aes(x = AGE_BAND_NEW, y = perc_each_class, fill = aware_class, group = aware_class)) + 
  geom_bar(stat = "identity") + 
  facet_grid(GENDER~.) + 
  theme_bw() + 
  labs(y = "percent of total rate of prescription by age group in each category (sense check!?)",
       title = "AWARE CLASSIFICATION")
