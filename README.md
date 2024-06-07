Code for working on UK antibiotic prescirption data - in progress. 

Files are (as of 7/6/2024): 

- combine_data_ICB.R. This reads in all the ICB data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later.

- combine_data_practice.R. This reads in all the practice data files and combines them. Splits date into month and year and converts age to a factor. for the columns UNIQUE_PATIENT_COUNT & ITEMS, the * (used for hidden values between 1 and 4) is converted to 1. This will be changed later. Note that there are a LOT of these hidden values.

- initial_data_look.R This contains Naomi's code currently used to look at the data, and is constantly changing.

- by_population_2023.R calculates the rate of items prescribed by ICB in 2023. Averaged across months. Excludes Wales, unknown demographics and indeterminate gender. 

The data can be accessed from https://opendata.nhsbsa.net/dataset/foi-01671, and needs to be put in subfolders of data to run the above code. (data > ICB_Data and data > Practice_Data).

The plots folder contains plots, currently from the by_population_2023.R file
