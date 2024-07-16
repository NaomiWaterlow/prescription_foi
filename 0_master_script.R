############ Master script ############################################################
####### July 2024 #####################################################################
####### Authors: Naomi Waterlow & Gwen Knight #########################################
#######################################################################################

# load required libraries
library(purrr)
library(stringr)
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggpubr)

#check we have appropiate folders for storing
dir.create(file.path("plots/"))
dir.create(file.path("data/"))


#### DO THE FIRST TIME ONLY, takes a while ######

# combine the individual datasets
source("0a_combine_data_ICB.R") # writes csv with combined data - ICB level
source("0b_combine_data_practice.R") # writes csv with combined data - GP level
# population data by ICB
source("0c_pop_data_2023_ICBs.R")

########## RUN ANALYSIS ###### 

# If sections 0 and 1 have been run previously, then each other numbered section can be ran 
# without rerunning everything. 

# investigate the data
source("1_initial_data_look_total.R") # creates Figure 1 and writes csv with cleaned total data
# Can also look by ICB (although this not used specifically in paper)
# source("1a_initial_data_look_by_ICB.R")

# run the STARPU analysis - Figure 2. 
source("2a_STARPU.R") # creates the updates to starpu
source("2b_STARPU_applied.R") # applies to the ICBs and creates figure 2. 

# run the AWARE analysis - Figure 3. 
source("3_AWARE.R")

# run the flu and mrsa analysis - Figure 4. 
source("4a_RTI.R") # flu analysis
source("4b_beta_lactams.R") # MRSA analysis and Fig 4. 





