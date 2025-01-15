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
library(tidyverse)


#check we have appropriate folders for storing
if(!file.exists("plots")){dir.create(file.path("plots/"))}
if(!file.exists("data")){dir.create(file.path("data/"))}
if(!file.exists("tables")){dir.create(file.path("tables/"))}


# choose which version to run for sensitivity of *s
# Options are: 
# "default_1" - stars set to 1
# "sens_4" - stars set to 4
# "m4_f1" - if male, stars set to 4, if female stars set to 1. Unknowns/Indeterminate set to 0
# "20_cutoff" - if  20 or under, stars set to 4, if over 20 stars set to 1. Unknowns set to 0
sensitivity_choice <- "default_1"
# if you want to create a new sensitivity analysis, it needs to be altered in scripts 0a, 0b and 1.

if(!file.exists(paste0("plots/", sensitivity_choice))){dir.create(file.path(paste0("plots/", sensitivity_choice)))}
if(!file.exists(paste0("data/", sensitivity_choice))){dir.create(file.path(paste0("data/", sensitivity_choice)))}

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





