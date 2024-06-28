# required packages
library(stringr)
library(data.table)
library(ggplot2)
# load in the data and mark those that are beta lactams
all_data <- fread("data/all_data.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
all_data[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_family := DRUG_FAMILY]
data_2017 <- all_data[YEAR == 2017]
data_2017_com <- data_2017[, sum(ITEMS), by = c("AGE_BAND", "GENDER", "drug_family")]
data_2017_com <- data_2017_com[AGE_BAND != "Unknown" & (GENDER != "Indeterminate" & GENDER != "Unknown")]

fwrite(data_2017_com, "foi_data_for_model.csv")
