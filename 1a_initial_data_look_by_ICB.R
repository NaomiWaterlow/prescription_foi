############ ICB data exploration ################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

###### initial look at the specific drugs #####


#create lookup
all_ICB_data <- fread(paste0("data/",sensitivity_choice,"/all_ICB_data_",sensitivity_choice,".csv"))
drugs_lookup <- fread("data/drugs_lookup.csv")
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]

all_ICB_data[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86-90", "91-95","96-100","101-105", "105+", "Unknown") )]

for(i in drugs_lookup$BNF_CHEMICAL_SUBSTANCE_CODE){
  
  target <- i 
 # target <- "0501013B0"
  target_name <- all_ICB_data[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  
  ICB_target <- all_ICB_data[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  ggplot(ICB_target[],aes(x= AGE_BAND, y = ITEMS, colour = YEAR) ) +
    geom_jitter( alpha =0.5) +
    labs(title = target_name) + facet_grid(.~YEAR)
  
  # # percentage of data rows that are in the 1-4 category
  prop_1_4 <- table(ICB_target$ITEMS == 1)["TRUE"] / dim(ICB_target)[1]
  print(paste0(target_name, "--- Proportion of data points with 1-4 items prescribed: ", round(prop_1_4,2)))

  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, p_1_4 := prop_1_4]
  
  # total number of rows in the original dataset
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_entries := dim(ICB_target)[1]]
  # total number of prescriptions
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_prescriptions := sum(ICB_target$ITEMS)]
  
  
}



