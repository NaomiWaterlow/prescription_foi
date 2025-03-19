##### Code for working on UK antibiotic prescirption data ########## 

## Published manuscript
Waterlow NR, Ashfield T, Knight GM. Observational study of antibiotic prescribing patterns by age and sex in primary care in England: why we need to take this variation into account to evaluate antibiotic stewardship and predict AMR variation. JAC Antimicrob Resist. 2025 Feb 7;7(1):dlae210. doi: 10.1093/jacamr/dlae210. PMID: 39927312; PMCID: PMC11803082.

## Authors: Naomi Waterlow & Gwen Knight
## Date: July 2024

## Data
The subnational data can be accessed from https://opendata.nhsbsa.net/dataset/foi-01671 along with the reference data tables. The national data can be accessed from a subsequent FOI request here: https://opendata.nhsbsa.net/dataset/foi-01975. Copyright NHSBA, open government lisence (https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). All datasets (csv files) need to be put in subfolders of data to run the above code. (data, data > ICB_Data and data > Practice_Data).

## Code 
- initial_data_look_total.R. This script loads the data that is not split by geography, calculates the population sizes and makes time series plots of each drug. Also generates Figure 1. This script must be run before the other scripts will work, as it generates the initial cleaned data. 
=======

All code can be run from 0_master_script.R, and this contains all the instructions on how to run it.


##### Old information - extra analyses not in the paper

- 0_initial_data_look_total.R. This script loads the data that is not split by geography, calculates the population sizes and makes plots of each drug. Also generates Figure 1. This script must be run before the other scripts will work, as it generates the initial cleaned data. 


- RTI.R This script compares the respiratory antibiotics to the data of flu vaccinations, option at the top to Exclude Amoxicillin or not. 

- beta-lactams.R: This compares the prescriptions of beta-lactams to the proportion resistance to MRSA. Creates Figure 4. 

- AWARE.R This assigns the aware classifications to the drugs and creates Figure 3. 

The following scripts use the geographically split data: 

- combine_data_ICB.R. This reads in all the ICB data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later.

- combine_data_practice.R. This reads in all the practice data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later. Note that there are a LOT of these hidden values.

- initial_data_look.R This contains Naomi's code currently used to look at the data, and is constantly changing.

- by_population_2023.R calculates the rate of items prescribed by ICB in 2023. Averaged across months. Excludes Wales, unknown demographics and indeterminate gender.
Also looks at seasonality in 2023

- GBS-ca_case_study.R a quick look at tGroup B strep resistance, not very interesting.

  





The plots folder contains output plots! 
