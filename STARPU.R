# STAR-PU analysis

# INitially just do updated one with all data. 

all_data_ex <- fread("data/all_data_organised.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")
pop_sizes_all <- fread("data/pop_sizes.csv")

data_sub <- all_data_ex[YEAR == 2023,]
pop_sub <- pop_sizes_all[YEAR == 2023]
data_2023 <- data_sub[, sum(ITEMS), by = c("AGE_BAND", "GENDER")]
data_2023[pop_sub, on = c("GENDER", "AGE_BAND"), pop := i.value]
data_2023[, rate := V1/pop]
base_rate <- data_2023[AGE_BAND=="61-65" & GENDER == "Female", "rate"]
data_2023[, base := base_rate]
data_2023[, star_pu := rate/base]


data_2023[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                      "6-10", "11-15", "16-20", "21-25", 
                                                      "26-30", "31-35", "36-40", "41-45", 
                                                      "46-50", "51-55",  "56-60", "61-65", 
                                                      "66-70", "71-75", "76-80",  "81-85" ,
                                                      "86+"))]


STAR_PU_NEW <- dcast.data.table(data_2023, AGE_BAND ~ GENDER, value.var = "star_pu")
STAR_PU_NEW[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                   "6-10", "11-15", "16-20", "21-25", 
                                                   "26-30", "31-35", "36-40", "41-45", 
                                                   "46-50", "51-55",  "56-60", "61-65", 
                                                   "66-70", "71-75", "76-80",  "81-85" ,
                                                   "86+"))]
NEW_STARPU <- ggplot(data_2023, aes( x = GENDER, y = AGE_BAND, fill = star_pu)) + 
  geom_tile() + geom_text(aes(label = round(star_pu,2)), colour = "white") + 
  theme_bw() + 
  labs(y = "Age Band", x = "Gender", fill = "STAR PU 2023")
            
