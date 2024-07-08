# applying the starpu
library(scales)


# load the pre-calculated starpus
starpu_by_drugs <- fread(file = "data/starpu_per_drug.csv")
starpu_overall <- fread(file = "data/starpu_overall.csv")

# load the cleaned ICB data

ICB_data <- fread("data/ICB_data/all_ICB_data.csv")
ICB_data_2023 <- ICB_data[YEAR == 2023]
drugs_lookup <- fread("data/drugs_lookup.csv")

#source("pop_data_2023_ICBs.R")
ICB_pops <- fread("data/ICB_Data/ICB_2023_pops.csv")

##### ICB STARPU POPS #####


# 1. overall

# x <- population[age,sex, ICB] * starpu[age,sex, ICB]
# sum(X[ICB]) == starpu pop pby ICB
ICB_pops[starpu_overall, on = c(AGE_BAND_NEW = "AGE_BAND", "GENDER"), 
         star_pu := i.star_pu]
ICB_pops[, star_pu_pop := star_pu * PEOPLE]
ICB_pops_overall <- ICB_pops[, sum(star_pu_pop), by = "ICB_CODE"]
colnames(ICB_pops_overall)[which(colnames(ICB_pops_overall) == "V1")] <- "starpu_pop"


# 2. by drug

ICB_pops <- fread("data/ICB_Data/ICB_2023_pops.csv")
ICB_pops_drugs <- data.table()

for(i in unique(starpu_by_drugs$drug_starpu)){
  print(i)
  starpu_temp <- starpu_by_drugs[drug_starpu == i,]
  
  ICB_pops[starpu_temp, on = c(AGE_BAND_NEW = "AGE_BAND", "GENDER"), 
           star_pu := i.star_pu]
  ICB_pops[, star_pu_pop := star_pu * PEOPLE]
  ICB_pops_temp <- ICB_pops[, sum(star_pu_pop), by = "ICB_CODE"]
  colnames(ICB_pops_temp)[which(colnames(ICB_pops_temp) == "V1")] <- "starpu_pop"
  
  ICB_pops_temp$drug_type <- i
  ICB_pops_drugs <- rbind(ICB_pops_drugs, ICB_pops_temp)
  
}



##### PRESCRIPTION DATA #####


# need to format to same age groups
ICB_data_2023[AGE_BAND == "86-90", AGE_BAND := "86+" ]
ICB_data_2023[AGE_BAND == "91-95", AGE_BAND := "86+" ]
ICB_data_2023[AGE_BAND == "96-100", AGE_BAND := "86+" ]
ICB_data_2023[AGE_BAND == "101-105", AGE_BAND := "86+" ]
ICB_data_2023[AGE_BAND == "105+", AGE_BAND := "86+" ]
# remove Unknownss
#drop those in the unknown or interderminate gender / age 
ICB_data_2023 <- ICB_data_2023[GENDER != "Indeterminate" & GENDER != "Unknown" & AGE_BAND != "Unknown"]

# need to add drug families
ICB_data_2023[drugs_lookup, on = c("BNF_CHEMICAL_SUBSTANCE_CODE"), drug_type := i.Drug_type]

# combine into total and across months, ages, gender. So total prescriptions by drug type
ICB_items_drugs <- ICB_data_2023[, sum(ITEMS), by = c("ICB_CODE", "drug_type")]
colnames(ICB_items_drugs)[which(colnames(ICB_items_drugs) == "V1")] <- "ITEMS"
# same but also across all drugs
ICB_items_overall <- ICB_data_2023[, sum(ITEMS), by = c("ICB_CODE")]
colnames(ICB_items_overall)[which(colnames(ICB_items_overall) == "V1")] <- "ITEMS"

## combine population and prescriptions and claculate items per starpu
ICB_items_overall[ICB_pops_overall, on = "ICB_CODE", starpu_pop := i.starpu_pop]
ICB_items_overall[, rating := ITEMS/starpu_pop]


# Same but by drug family
ICB_items_drugs[ICB_pops_drugs, on = c("ICB_CODE", "drug_type"), starpu_pop := i.starpu_pop]
ICB_items_drugs[, rating := ITEMS/starpu_pop]



ggplot(ICB_items_drugs, aes(x = drug_type, y = rating, colour = ICB_CODE)) + 
  geom_jitter() + theme_bw() + 
  labs(x = "Drug Family", y = "Prescriptions per STARPU population", colour = "ICB Code")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Need to normalise by drug value, to see outliers
# remove NAs (from Wales)
ICB_items_drugs <- ICB_items_drugs[!is.na(starpu_pop)]
ICB_items_overall <- ICB_items_overall[!is.na(starpu_pop)]

ICB_items_drugs[, mean_rating := mean(rating), by= "drug_type"]
ICB_items_drugs[, normalised_rating := rating / mean_rating]

ICB_items_overall[, mean_rating := mean(rating)]
ICB_items_overall[, normalised_rating := rating / mean_rating]


ICB_items_drugs[drug_type == "Penicillins", short_title :=  "Penicillins"]
ICB_items_drugs[drug_type == "Macrolides", short_title :=  "Macrolides"]
ICB_items_drugs[drug_type == "Cephalosportins and other beta-lactams", short_title :=  "Ceph's"]
ICB_items_drugs[drug_type == "Quinolones", short_title :=  "Quinolones"]
ICB_items_drugs[drug_type == "Clindamycin and lincomycin", short_title :=  "C&L"]
ICB_items_drugs[drug_type == "Sulfonamides and trimethoprim", short_title :=  "S&T"]
ICB_items_drugs[drug_type == "Some other antibacterials", short_title :=  "Other"]
ICB_items_drugs[drug_type == "Antileprotic drugs", short_title :=  "Lep"]
ICB_items_drugs[drug_type == "Tetracyclines", short_title :=  "Tetracyclines"]
ICB_items_drugs[drug_type == "Antituberculosis drugs", short_title :=  "TB"]
ICB_items_drugs[drug_type == "Urinary-tract infections", short_title :=  "UTIs"]
ICB_items_drugs[drug_type == "Metronidazole, tinidazole and ornidazole", short_title :=  "MTO"]
ICB_items_drugs[drug_type == "Aminoglycosides", short_title :=  "Aminoglycosides"]



STAR_PU_APPLIED <- ggplot(ICB_items_drugs[ICB_CODE != "QXU" & ICB_CODE !="QJM" & ICB_CODE != "QOP"], aes(x = short_title, y = log(normalised_rating))) + 
  geom_blank(aes(y = 0, ymin =-abs(normalised_rating)*0.5 , ymax = abs(normalised_rating)*0.5) ) +
  geom_hline(yintercept = log(ICB_items_overall$normalised_rating), alpha = 0.2) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QXU",normalised_rating]),colour = "#D81B60" ,alpha = 1, size = 1) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QJM",normalised_rating]),colour = "#1E88E5" ,alpha = 1, size = 1) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QOP",normalised_rating]),colour = "#FFC107" ,alpha = 1, size = 1) +
    geom_jitter(width = 0.2, colour = "grey20") + theme_bw() +
  facet_wrap(.~short_title, scales = "free",ncol = 5) + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QUY"], aes(x = short_title, y = log(normalised_rating)), colour = "#D81B60") + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QJM"], aes(x = short_title, y = log(normalised_rating)), colour = "#1E88E5") + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QOP"], aes(x = short_title, y = log(normalised_rating)), colour = "#FFC107") + 
  labs(x = "Drug Family", y = "Normalised prescriptions per STARPU population (log-scale)", colour = "ICB Code")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank())


FIG2 <- grid.arrange(NEW_STARPU, STAR_PU_APPLIED, layout_matrix = rbind(c(1,1,2,2,2),
                                                                        c(1,1,2,2,2)))

ggsave(paste0("plots/Fig2.pdf"), plot = FIG2, 
       width = 20, height = 10)


#  31 March 2023, the number of ICBs meeting the national target for total
# primary care prescribing of antibiotics ‘at or less than 0.871 items per STAR-PU’ was 7 out of 42
# (17%) and the number of ICBs meeting the national target for primary care broad-spectrum
# antibiotic prescribing ‘at or less than 10%’ was 41 out of 42 (98%). 

# From https://assets.publishing.service.gov.uk/media/65cf498ee1bdec001132225c/ESPAUR-report-2022-to-2023.pdf



ggplot(ICB_items_overall, aes(x = ICB_CODE, y = rating)) + 
  geom_point() + theme_bw() + 
  labs(y = "ITEMS per STAR-PU")

