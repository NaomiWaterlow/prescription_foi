############ STARPU Application ################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

# load the pre-calculated starpus
starpu_by_drugs <- fread(file = paste0("data/",sensitivity_choice,"/starpu_per_drug_",sensitivity_choice,".csv"))
starpu_overall <- fread(file = paste0("data/",sensitivity_choice,"/starpu_overall_",sensitivity_choice,".csv"))

# load the cleaned ICB data
ICB_data <- fread(paste0("data/",sensitivity_choice,"/all_ICB_data_",sensitivity_choice,".csv"))
# 2023 obnly
ICB_data_2023 <- ICB_data[YEAR == 2023]
#remove wales as not in the combined data set
ICB_data_2023 <- ICB_data_2023[ICB_CODE != "Q99"]
drugs_lookup <- fread("data/drugs_lookup.csv")
# load in the ICB population data
ICB_pops <- fread("data/ICB_Data/ICB_2023_pops.csv")

##### ICB STARPU POPS #####


# 1. overall

# add the starpu values to the population sizes data frame
ICB_pops[starpu_overall, on = c(AGE_BAND_NEW = "AGE_BAND", "GENDER"), 
         star_pu := i.star_pu]
# times 
ICB_pops[, star_pu_pop := star_pu * PEOPLE]
# sum the star_pu population by ICB Code
ICB_pops_overall <- ICB_pops[, sum(star_pu_pop), by = "ICB_CODE"]
# rename column
colnames(ICB_pops_overall)[which(colnames(ICB_pops_overall) == "V1")] <- "starpu_pop"


# 2. by drug

# create data.table for storage
ICB_pops_drugs <- data.table()

#for each drug family
for(i in unique(starpu_by_drugs$drug_starpu)){
  #print what doing
  print(i)
  # subset to that drug family
  starpu_temp <- starpu_by_drugs[drug_starpu == i,]
  # add starpu to population
  ICB_pops[starpu_temp, on = c(AGE_BAND_NEW = "AGE_BAND", "GENDER"), 
           star_pu := i.star_pu]
  # multiply
  ICB_pops[, star_pu_pop := star_pu * PEOPLE]
  # sum 
  ICB_pops_temp <- ICB_pops[, sum(star_pu_pop), by = "ICB_CODE"]
  #rename
  colnames(ICB_pops_temp)[which(colnames(ICB_pops_temp) == "V1")] <- "starpu_pop"
  #store
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


# have a look
ggplot(ICB_items_drugs, aes(x = drug_type, y = rating, colour = ICB_CODE)) + 
  geom_jitter() + theme_bw() + 
  labs(x = "Drug Family", y = "Prescriptions per STARPU population", colour = "ICB Code")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Need to normalise by drug value, to see outliers
# remove NAs (from Wales)
ICB_items_drugs <- ICB_items_drugs[!is.na(starpu_pop)]
ICB_items_overall <- ICB_items_overall[!is.na(starpu_pop)]
# calculate mean and divide each row by it
ICB_items_drugs[, mean_rating := mean(rating), by= "drug_type"]
ICB_items_drugs[, normalised_rating := rating / mean_rating]
# same for overall values
ICB_items_overall[, mean_rating := mean(rating)]
ICB_items_overall[, normalised_rating := rating / mean_rating]

# Create shorter titles for plot
ICB_items_drugs[drug_type == "Penicillins", short_title :=  "Penicillins"]
ICB_items_drugs[drug_type == "Macrolides", short_title :=  "Macrolides"]
ICB_items_drugs[drug_type == "Cephalosporins and other beta-lactams", short_title :=  "Ceph's"]
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

# calculate total prescriptions by category
ICB_items_drugs[, drug_total := sum(ITEMS), by ="drug_type"]
# combine drug family name and total prescriptions for pretty plotting
ICB_items_drugs[, new_label := paste0(drug_type, " (n=", format(drug_total, big.mark=",", scientific=F, trim = T), ")")]

#create plot
STAR_PU_APPLIED <- ggplot(ICB_items_drugs[ICB_CODE != "QXU" & ICB_CODE !="QJM" & ICB_CODE != "QOP"], aes(x = short_title, y = log(normalised_rating))) + 
  geom_blank(aes(y = 0, ymin =-abs(normalised_rating)*0.5 , ymax = abs(normalised_rating)*0.5) ) +
  geom_hline(yintercept = log(ICB_items_overall$normalised_rating), alpha = 0.2) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QXU",normalised_rating]),colour = "#D81B60" ,alpha = 1, size = 1) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QJM",normalised_rating]),colour = "#1E88E5" ,alpha = 1, size = 1) +
  geom_hline(yintercept = log(ICB_items_overall[ICB_CODE == "QOP",normalised_rating]),colour = "#FFC107" ,alpha = 1, size = 1) +
    geom_jitter(width = 0.2, colour = "grey20") + theme_bw() +
  facet_wrap(.~new_label, scales = "free",nrow=2, labeller = label_wrap_gen(width=20)) + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QUY"], aes(x = short_title, y = log(normalised_rating)), colour = "#D81B60", size =3) + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QJM"], aes(x = short_title, y = log(normalised_rating)), colour = "#1E88E5", size =3) + 
  geom_point(data = ICB_items_drugs[ICB_CODE == "QOP"], aes(x = short_title, y = log(normalised_rating)), colour = "#FFC107", size =3) + 
  labs(x = "Drug Family", y = "Normalised prescriptions per UCM population (log-scale)",
       colour = "ICB Code")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank())




ggplot(ICB_items_overall, aes(x = ICB_CODE, y = rating)) + 
  geom_point() + theme_bw() + 
  labs(y = "ITEMS per STAR-PU")


# work out rank changes

ICB_items_overall[, ranking := rank(rating)]
ICB_items_drugs[,ranking := rank(rating), by = "drug_type"]

# create matching columsn so can include in same plot

ICB_items_overall_match <- data.frame(
  ICB_CODE = ICB_items_overall[,"ICB_CODE"], 
  drug_type = "Overall", 
  rating = ICB_items_overall[,"rating"], 
  short_title = "Overall", 
  ranking = ICB_items_overall[,"ranking"]
)

starpu_rankings <- rbind(ICB_items_drugs[,c("ICB_CODE", "drug_type",
                                            "rating", "short_title", 
                                            "ranking")],
                         ICB_items_overall_match)

# make OVERALL first factor
starpu_rankings$short_title <- factor(starpu_rankings$short_title,
                                      levels = c("Overall","Penicillins",
                                                 "Ceph's",
                                                 "Tetracyclines" ,
                                                 "Aminoglycosides",
                                                 "Macrolides",
                                                 "C&L",
                                                 "Other",
                                                 "S&T",
                                                 "TB", 
                                                 "Lep", 
                                                 "MTO",
                                                 "Quinolones",
                                                 "UTIs" ))
# order factor by overall ranking
temp <- starpu_rankings[drug_type == "Overall",]
levels_to_use <- temp[order(ranking)]$ICB_CODE
starpu_rankings$ICB_CODE <- factor(starpu_rankings$ICB_CODE, levels = rev(levels_to_use))

#want to label whether it's up or down for colouring
starpu_rankings[ICB_items_overall_match, on = "ICB_CODE", overal_ranking := i.ranking]
starpu_rankings[, change := overal_ranking - ranking]
starpu_rankings[change == 0, change_label := "No change"]
starpu_rankings[change > 0, change_label := "Increased"]
starpu_rankings[change < 0, change_label := "Decreased"]


starpu_rankings[ranking <= 10, rank_group := 1]
starpu_rankings[ranking > 10 & ranking <=20, rank_group := 2]
starpu_rankings[ranking > 20 & ranking <=30, rank_group := 3]
starpu_rankings[ranking > 30  ,rank_group := 4]

starpu_rankings$rank_group <- factor(starpu_rankings$rank_group, 
                                        levels = c(1,2,3,4))


STARPU_RANKINGS <- ggplot(starpu_rankings, aes(x = short_title, y = ICB_CODE, fill = rank_group)) + 
                            geom_tile() + 
  theme_bw() +
 # scale_fill_gradientn(colours = c("#24693D", "#F4F8FB", "#2A5783"), limits = c(-42,42)) + 
  labs(x = "Updated Comparison Metric group", y = "ICB Code", fill = "Rank group", 
         title = "C: Ranking of UCMs") + 
  scale_fill_brewer(palette = "Purples") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

STARPU_RANKINGS_SUP <- ggplot(starpu_rankings, aes(x = short_title, y = ICB_CODE, fill = change)) + 
  geom_tile() + geom_text(aes(label = ranking)) + 
  theme_bw() +
  scale_fill_gradientn(colours = c("#24693D", "#F4F8FB", "#2A5783"), limits = c(-42,42)) + 
  labs(x = "Updated Comparison Metric group", y = "ICB Code", fill = "Change in rank", 
       title = "Ranking of UCMs")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(paste0("plots/",sensitivity_choice,"/Starpu_supplement_",sensitivity_choice,".pdf"), 
       plot = STARPU_RANKINGS_SUP, 
       width = 20, height = 10)



ß# combine all to Figure 2
FIG2 <- grid.arrange(NEW_STARPU + theme(legend.position = "None"),
                     STAR_PU_APPLIED + labs(title = "D: Comparison of overall vs family specific UCMs"),
                     OLD_STARPU + theme(legend.position = "None"),
                     LEG,
                     STARPU_RANKINGS,
                     layout_matrix = rbind(c(3,3,1,1,4,5,5,5,5,5),
                                           c(3,3,1,1,4,5,5,5,5,5),
                                           c(2,2,2,2,2,2,2,2,2,2),
                                           c(2,2,2,2,2,2,2,2,2,2)))
#save
ggsave(paste0("plots/",sensitivity_choice,"/Fig2_",sensitivity_choice,".pdf"), plot = FIG2, 
       width = 20, height = 10)


#  31 March 2023, the number of ICBs meeting the national target for total
# primary care prescribing of antibiotics ‘at or less than 0.871 items per STAR-PU’ was 7 out of 42
# (17%) and the number of ICBs meeting the national target for primary care broad-spectrum
# antibiotic prescribing ‘at or less than 10%’ was 41 out of 42 (98%). 

# From https://assets.publishing.service.gov.uk/media/65cf498ee1bdec001132225c/ESPAUR-report-2022-to-2023.pdf


