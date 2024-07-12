library(scales)
library(data.table)
library(ggplot2)
library(gridExtra)
library(stringr)
library(ggpubr)

# STAR-PU analysis
dir.create(file.path("plots/starpu"))
# INitially just do updated one with all data. 

all_data_ex <- fread("data/all_data_organised.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
pop_sizes_all <- fread("data/pop_sizes.csv")

data_sub <- all_data_ex[YEAR == 2023,]
pop_sub <- pop_sizes_all[YEAR == 2023]
data_2023 <- data_sub[, sum(ITEMS), by = c("AGE_BAND", "GENDER")]
data_2023[pop_sub, on = c("GENDER", "AGE_BAND"), pop := i.value]
data_2023[, rate := V1/pop]
base_rate <- data_2023[AGE_BAND=="66-70" & GENDER == "Female", "rate"]
data_2023[, base := base_rate]
data_2023[, star_pu := rate/base]


data_2023[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                      "6-10", "11-15", "16-20", "21-25", 
                                                      "26-30", "31-35", "36-40", "41-45", 
                                                      "46-50", "51-55",  "56-60", "61-65", 
                                                      "66-70", "71-75", "76-80",  "81-85" ,
                                                      "86+"))]
fwrite(data_2023, file = "data/starpu_overall.csv")

STAR_PU_NEW <- dcast.data.table(data_2023, AGE_BAND ~ GENDER, value.var = "star_pu")
STAR_PU_NEW[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                   "6-10", "11-15", "16-20", "21-25", 
                                                   "26-30", "31-35", "36-40", "41-45", 
                                                   "46-50", "51-55",  "56-60", "61-65", 
                                                   "66-70", "71-75", "76-80",  "81-85" ,
                                                   "86+"))]
NEW_STARPU<- ggplot(data_2023, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
  geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "grey14") + 
  theme_bw() + 
  labs(y = "Age Band", x = "Gender", fill = "value", title = "Updated measure") + 
  scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" , midpoint = 1, 
                       limits=c(0,2)) 


old_starpu <- data.table(
  AGE_BAND = rep(c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", 
               "55-64", "65-74", "75+"),2), 
  GENDER = c(rep("Male",9), rep("Female", 9)),
  star_pu = c(0.8,0.3,0.3,0.2,0.3, 0.3, 0.4, 0.7, 1, 
              0.8, 0.4, 0.6, 0.6, 0.6, 0.6, 0.7, 1.0, 1.3)
)

old_starpu$AGE_BAND <- factor(old_starpu$AGE_BAND, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", 
                                                              "55-64", "65-74", "75+"))
LEG <- get_legend(NEW_STARPU) 

OLD_STARPU <- ggplot(old_starpu, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
  geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "grey14") + 
  theme_bw() + 
  labs(y = "Age Band", x = "Gender", fill = "value", title = "2013 STAR-PU") + 
  scale_fill_gradient2(low = "#24693D", mid = "#F4F8FB",high = "#2A5783" , midpoint = 1, 
                       limits=c(0,2)) 

# # Combine all drugs that each have less than a total of 10K prescrptions 
# data_2023_drugs[, total_prescrips := sum(V1), by = drug_name]
# data_2023_drugs[total_prescrips < 10000, drug_starpu := "Other"]
# data_2023_drugs[total_prescrips >= 10000, drug_starpu := drug_name]

# Combine based on drug type bnf
data_sub[drugs_lookup, on = c(drug_name = "CHEMICAL_SUBSTANCE_BNF_DESCR"), drug_starpu := i.Drug_type]


# sum other category together
data_2023_drugs <- data_sub[,sum(ITEMS), by = c("AGE_BAND", "GENDER", "drug_starpu")]

data_2023_drugs[pop_sub, on = c("GENDER", "AGE_BAND"), pop := i.value]
data_2023_drugs[, rate := V1/pop]


base_rate_drugs <- data_2023_drugs[AGE_BAND=="66-70" & GENDER == "Female", c("rate", "drug_starpu")]
data_2023_drugs[base_rate_drugs, on = c("drug_starpu"), base := i.rate]
data_2023_drugs[, star_pu := rate/base]

fwrite(data_2023_drugs, file = "data/starpu_per_drug.csv")

all_drugs <- unique(data_2023_drugs$drug_starpu)

template <- unique(all_data_ex[,c("GENDER", "AGE_BAND")])




# set defaults for kids
template[AGE_BAND %in% c("0-1", "2-5", 
                         "6-10", "11-15"), star_pu := 0.01]



for(i in all_drugs){
  
  temp <- data_2023_drugs[drug_starpu == i,]
  temp2 <- template
  temp2[temp, on = c("AGE_BAND", "GENDER"), star_pu := i.star_pu]  
  
  # set limits of 0.01 and 100
  temp2[star_pu < 0.01, star_pu := 0.01]
  temp2[star_pu > 100, star_pu := 100]
  
  temp2[, GENDER := factor(GENDER, levels = c("Female", "Male"))]
  temp2[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                     "6-10", "11-15", "16-20", "21-25", 
                                                     "26-30", "31-35", "36-40", "41-45", 
                                                     "46-50", "51-55",  "56-60", "61-65", 
                                                     "66-70", "71-75", "76-80",  "81-85" ,
                                                     "86+"))]
  
  
  
  PLOT_TEMP <- ggplot(temp2, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
    geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "white") + 
    theme_bw() + 
    labs(y = "Age Band", x = "Gender", fill = "STAR PU 2023", title = paste0(i, ": total prescriptions", sum(temp$V1)))+ 
    scale_y_discrete(drop = F) + 
    scale_x_discrete(drop = F) 
  
  ggsave(filename = paste0("plots/starpu/",str_replace_all(i, "[^[:alnum:]]", " "),
                "_new_starpu.pdf"), plot = PLOT_TEMP, 
         width = 20, height = 10) 
}
            
