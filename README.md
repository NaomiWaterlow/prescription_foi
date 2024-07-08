Code for working on UK antibiotic prescirption data - in progress. 

- initial_data_look_total.R. This script loads the data that is not split by geography, calculates the population sizes and makes plots of each drug. Also generates Figure 1. This script must be run before the other scripts will work, as it generates the initial cleaned data. 

- RTI.R This script compares the respiratory antibiotics to the data of flu vaccinations, option at the top to Exclude Amoxicillin or not. 

- beta-lactams.R: This compares the prescriptions of beta-lactams to the proportion resistance to MRSA. Creates Figure 4. 

- AWARE.R This assigns the aware classifications to the drugs and creates Figure 3. 



The following scripts use the geographically split data (and is less up to date): 


- combine_data_ICB.R. This reads in all the ICB data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later.

- combine_data_practice.R. This reads in all the practice data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later. Note that there are a LOT of these hidden values.

- initial_data_look.R This contains Naomi's code currently used to look at the data, and is constantly changing.

- by_population_2023.R calculates the rate of items prescribed by ICB in 2023. Averaged across months. Excludes Wales, unknown demographics and indeterminate gender.
Also looks at seasonality in 2023

- GBS-ca_case_study.R a quick look at tGroup B strep resistance, not very interesting.

  




The data can be accessed from https://opendata.nhsbsa.net/dataset/foi-01671, and needs to be put in subfolders of data to run the above code. (data > ICB_Data and data > Practice_Data).

The plots folder contains output plots! 
