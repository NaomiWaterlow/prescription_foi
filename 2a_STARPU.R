############ STARPU Analysis ################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

###### SETUP ######

# Create folder for plots if doesn't exist
if(!file.exists(paste0("plots/", sensitivity_choice, "/starpu"))){
  dir.create(file.path(paste0("plots/", sensitivity_choice, "/starpu")))}

#read in the data 
all_data_ex <- fread(paste0("data/",sensitivity_choice,"/all_data_organised_",sensitivity_choice,".csv"))
# read in the lookup file
drugs_lookup <- fread("data/drugs_lookup.csv")
# read in the population sizes
pop_sizes_all <- fread("data/pop_sizes.csv")

# subset 2023 only for this analysis
data_sub <- all_data_ex[YEAR == 2023,]
pop_sub <- pop_sizes_all[YEAR == 2023]


####### Calculate Updated Compariosn metric #######

# calculate total items in 2023 (i.e. across drugs and months)
data_2023 <- data_sub[, sum(ITEMS), by = c("AGE_BAND", "GENDER")]
# add the population sizes
data_2023[pop_sub, on = c("GENDER", "AGE_BAND"), pop := i.value]
# calculate rate ber population
data_2023[, rate := V1/pop]
# specify the base rate for comparison
base_rate <- data_2023[AGE_BAND=="66-70" & GENDER == "Female", "rate"]
data_2023[, base := base_rate]
# work out relative rates
data_2023[, star_pu := rate/base]
# save 
fwrite(data_2023, file = paste0("data/",sensitivity_choice,"/starpu_overall_",sensitivity_choice,".csv"))

#make wide fprmat
STAR_PU_NEW <- dcast.data.table(data_2023, AGE_BAND ~ GENDER, value.var = "star_pu")

# check age band is factor with levls in correct order
data_2023[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                   "6-10", "11-15", "16-20", "21-25", 
                                                   "26-30", "31-35", "36-40", "41-45", 
                                                   "46-50", "51-55",  "56-60", "61-65", 
                                                   "66-70", "71-75", "76-80",  "81-85" ,
                                                   "86+"))]
# Make a plot
NEW_STARPU<- ggplot(data_2023, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
  geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "grey14") + 
  theme_bw() + 
  labs(y = "Age Band", x = "Gender", fill = "value", title = "B: Updated Comparison Metric") + 
  scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" , midpoint = 1, 
                       limits=c(0,2)) 

# enter the 2013 starpu for compariosn plot
old_starpu <- data.table(
  AGE_BAND = rep(c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", 
               "55-64", "65-74", "75+"),2), 
  GENDER = c(rep("Male",9), rep("Female", 9)),
  star_pu = c(0.8,0.3,0.3,0.2,0.3, 0.3, 0.4, 0.7, 1, 
              0.8, 0.4, 0.6, 0.6, 0.6, 0.6, 0.7, 1.0, 1.3)
)

# factor with levels in right order
old_starpu$AGE_BAND <- factor(old_starpu$AGE_BAND, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", 
                                                              "55-64", "65-74", "75+"))
#extract the legend
LEG <- get_legend(NEW_STARPU) 

#starpu old plot
OLD_STARPU <- ggplot(old_starpu, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
  geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "grey14") + 
  theme_bw() + 
  labs(y = "Age Band", x = "Gender", fill = "value", title = "A: 2013 STAR-PU") + 
  scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" , midpoint = 1, 
                       limits=c(0,2)) 


####### DRUG SPECIFIC UCM #####

# Add the drug families into the data frame
data_sub[drugs_lookup, on = c(drug_name = "CHEMICAL_SUBSTANCE_BNF_DESCR"), drug_starpu := i.Drug_type]

# sum together including by group (across month and drug family)
data_2023_drugs <- data_sub[,sum(ITEMS), by = c("AGE_BAND", "GENDER", "drug_starpu")]

#add population sizes
data_2023_drugs[pop_sub, on = c("GENDER", "AGE_BAND"), pop := i.value]
# calculate rate
data_2023_drugs[, rate := V1/pop]
# specifiy base rate
base_rate_drugs <- data_2023_drugs[AGE_BAND=="66-70" & GENDER == "Female", c("rate", "drug_starpu")]
# add the base rate in
data_2023_drugs[base_rate_drugs, on = c("drug_starpu"), base := i.rate]
# calculate the realtive rate
data_2023_drugs[, star_pu := rate/base]
# save the data file
fwrite(data_2023_drugs, file = paste0("data/",sensitivity_choice,"/starpu_per_drug_",sensitivity_choice,".csv"))
# list of all drugs
all_drugs <- unique(data_2023_drugs$drug_starpu)
# create template for filling in with each drug family specific UCM
template <- unique(all_data_ex[,c("GENDER", "AGE_BAND")])

# set defaults for kids (i.e. if no data)
template[AGE_BAND %in% c("0-1", "2-5", 
                         "6-10", "11-15"), star_pu := 0.01]

# for each drug type
for(i in all_drugs){
  
  # subset just that drug type
  temp <- data_2023_drugs[drug_starpu == i,]
  # copy template
  temp2 <- template
  # fill template with current dat
  temp2[temp, on = c("AGE_BAND", "GENDER"), star_pu := i.star_pu]  
  
  # set limits of 0.01 and 100
  temp2[star_pu < 0.01, star_pu := 0.01]
  temp2[star_pu > 100, star_pu := 100]
  # check correct format
  temp2[, GENDER := factor(GENDER, levels = c("Female", "Male"))]
  temp2[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                     "6-10", "11-15", "16-20", "21-25", 
                                                     "26-30", "31-35", "36-40", "41-45", 
                                                     "46-50", "51-55",  "56-60", "61-65", 
                                                     "66-70", "71-75", "76-80",  "81-85" ,
                                                     "86+"))]
  
  
  # create plot
  PLOT_TEMP <- ggplot(temp2, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
    geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "grey14") + 
    theme_bw() + 
    labs(y = "Age Band", x = "Gender", fill = "STAR PU 2023", title = paste0(i, ": total prescriptions ", sum(temp$V1)))+ 
    scale_y_discrete(drop = F) + 
    scale_x_discrete(drop = F) + 
    scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" , midpoint = 1) 
  # save plot
  ggsave(filename = paste0("plots/",sensitivity_choice,"/starpu/",str_replace_all(i, "[^[:alnum:]]", " "),
                "_new_starpu_",sensitivity_choice,".pdf"), plot = PLOT_TEMP, 
         width = 20, height = 10) 
  
  assign(paste0("PLOT_",i), PLOT_TEMP)
}

PLOT1 <- grid.arrange(PLOT_Aminoglycosides,`PLOT_Antileprotic drugs`, `PLOT_Antituberculosis drugs`, 
             `PLOT_Cephalosporins and other beta-lactams`)

ggsave(filename = paste0("plots/",sensitivity_choice,"/starpu/",
                         "UCM_family_1",sensitivity_choice,".pdf"), plot = PLOT1, 
       width = 20, height = 10)   

PLOT2 <- grid.arrange(`PLOT_Clindamycin and lincomycin`, PLOT_Macrolides, `PLOT_Metronidazole, tinidazole and ornidazole`, PLOT_Penicillins)

ggsave(filename = paste0("plots/",sensitivity_choice,"/starpu/",
                         "UCM_family_2",sensitivity_choice,".pdf"), plot = PLOT2, 
       width = 20, height = 10)   

PLOT3 <- grid.arrange(PLOT_Quinolones, `PLOT_Some other antibacterials`, 
                      `PLOT_Sulfonamides and trimethoprim`, PLOT_Tetracyclines, 
                      `PLOT_Urinary-tract infections`)

ggsave(filename = paste0("plots/",sensitivity_choice,"/starpu/",
                         "UCM_family_3",sensitivity_choice,".pdf"), plot = PLOT3, 
       width = 20, height = 10)   
