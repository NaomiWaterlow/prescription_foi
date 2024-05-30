# Look at data

library(ggplot2)


###### initial look at the specific drugs #####

## NOTE: this is in progress - and the denominator for the percentage does not include suubgroups who have no prescriptions


#create lookup
drugs_lookup <- fread("data/drugs_lookup.csv")
all_ICB_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]

for(i in drugs_lookup$BNF_CHEMICAL_SUBSTANCE_CODE){
  
  target <- i 
  target <- "0501013B0"
  target_name <- all_ICB_data[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  
  all_ICB_data <- all_ICB_data[!(GENDER == "Unknown")]
  
  ICB_target <- all_ICB_data[BNF_CHEMICAL_SUBSTANCE_CODE == target]
  
  ggplot(ICB_target[GENDER != "Indeterminate"],aes(x= AGE_BAND, y = ITEMS, colour = GENDER) ) +
    geom_jitter( alpha =0.5) +
    labs(title = target_name) + facet_grid(.~GENDER)
  
  # percentage of data rows that are in the 1-4 category
  prop_1_4 <- table(ICB_target$ITEMS == -100)["TRUE"] / dim(ICB_target)[1]
  print(paste0(target_name, "--- Proportion of data points with 1-4 items prescribed: ", round(prop_1_4,2)))
  
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, p_1_4 := prop_1_4]
  drugs_lookup[BNF_CHEMICAL_SUBSTANCE_CODE == target, total_entries := dim(ICB_target)[1]]
  
  
}


