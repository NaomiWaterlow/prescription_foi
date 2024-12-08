# ICB populations. 

dir.create(file.path("plots/byicb"))
# FOR GEOGRAPHIC VARIATION
# - excludes Wales
# - 2023 only
# - combines age-groups over 85 into one age group
# - excludes all Unknown categories (3.3%) as we don't know what the pop size for these is
# - excludes "Indeterminate" Gender, as don't know population size.

# Because need to combine age bands, have excluded UNIQUE_PATIENT_COLUMN as we can't use this without risking double counting
all_ICB_data <- fread("data/ICB_Data/all_ICB_data.csv")
drugs_lookup <- fread("data/drugs_lookup.csv")

# looking only at 2023 'post-covid' as this is also the year we have population data for
# not including wales (For now), as the population size dataset doesn't include it
# combine age_groups over 85
ICB_data_2023 <- all_ICB_data[YEAR==2023 & ICB_CODE != "Q99" & 
                                GENDER != "Unknown" & AGE_BAND != "Unknown" &
                                GENDER != "Indeterminate"]
ICB_data_2023[, UNIQUE_PATIENT_COUNT := NULL]

ICB_data_2023[, AGE_BAND_NEW := AGE_BAND]
ICB_data_2023[AGE_BAND == "86-90", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "91-95", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "96-100", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "101-105", AGE_BAND_NEW := "86+"]
ICB_data_2023[AGE_BAND == "105+", AGE_BAND_NEW := "86+"]

ICB_data_2023 <- dcast.data.table(ICB_data_2023, YEAR_MONTH + ICB_CODE + BNF_CHEMICAL_SUBSTANCE_CODE + GENDER + YEAR + MONTH + AGE_BAND_NEW~. , 
                 value.var = "ITEMS", fun.aggregate = sum)
colnames(ICB_data_2023)[which(colnames(ICB_data_2023) == ".")] <- "ITEMS"
ICB_colours <- set_names(rainbow(42), unique(ICB_data_2023$ICB_CODE))

#source("pop_data_2023_ICBs.R")
ICB_pops <- fread("data/ICB_Data/ICB_2023_pops.csv")

# add the population data
ICB_data_2023[pop_dat_combo, on = c("ICB_CODE", "AGE_BAND_NEW", "GENDER"), POP :=i.PEOPLE]
ICB_data_2023[, rate_per_1000 := (ITEMS/POP)*1000]

# add in drug lookup 
drugs_lookup <- fread("data/drugs_lookup.csv")
ICB_data_2023[drugs_lookup, on="BNF_CHEMICAL_SUBSTANCE_CODE", drug_name := CHEMICAL_SUBSTANCE_BNF_DESCR]
fwrite(ICB_data_2023, "data/2023_per_population.csv")
# save these for each drug

# Lets take a look!

# only looking at drugs where there were at least 500 prescriptions in the year
drugs_of_interest <- ICB_data_2023[,sum(ITEMS), by = "BNF_CHEMICAL_SUBSTANCE_CODE"][V1 >=500]$BNF_CHEMICAL_SUBSTANCE_CODE

ICB_data_2023$ICB_CODE <- factor(ICB_data_2023$ICB_CODE)
ICB_data_2023[, AGE_BAND_NEW := factor(AGE_BAND_NEW, levels = c("0-1", "2-5", 
                                                       "6-10", "11-15", "16-20", "21-25", 
                                                       "26-30", "31-35", "36-40", "41-45", 
                                                       "46-50", "51-55",  "56-60", "61-65", 
                                                       "66-70", "71-75", "76-80",  "81-85" ,
                                                       "86+") )]
Age_colours <- set_names(rainbow(19), unique(ICB_data_2023$AGE_BAND_NEW))

for(i in drugs_of_interest){
target <- i
target_name <- ICB_data_2023[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
ICB_target_2023 <- ICB_data_2023[BNF_CHEMICAL_SUBSTANCE_CODE == target]

# average per month
ICB_target_2023[, av_rate_per_1000:= mean(rate_per_1000), by = c("ICB_CODE", "GENDER", "AGE_BAND_NEW" )]
ICB_target_2023_av_month <- unique(ICB_target_2023[, c("YEAR_MONTH", "MONTH", "ITEMS", "rate_per_1000", "POP") := NULL])


PLOT_TEMP <- ggplot(ICB_target_2023_av_month, aes(x = AGE_BAND_NEW, y = av_rate_per_1000, colour = ICB_CODE, group =ICB_CODE)) + 
  geom_line() + facet_wrap(GENDER~.) + 
  labs(title = paste0(target_name, " (colours = ICBs)"), x = "Age Group", y = "Average monthly rate per 1000 people") + 
  theme_bw() + 
  scale_color_manual(values = ICB_colours, drop = F) + 
  theme(axis.text.y = element_text(size =10)  ,legend.position = "none"
        )

ggsave(paste0("plots/byicb/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_av_monhtly_rate_items.pdf"), plot = PLOT_TEMP)


#differences between gender by age
sex_dif <- dcast.data.table(ICB_target_2023_av_month,
                            ICB_CODE + BNF_CHEMICAL_SUBSTANCE_CODE + YEAR + AGE_BAND_NEW + drug_name ~ GENDER, value.var = "av_rate_per_1000")
sex_dif[, sex_diference := Female - Male]


PLOT_TEMP2 <- ggplot(sex_dif, aes(x = AGE_BAND_NEW, y = sex_diference, colour = ICB_CODE, group =ICB_CODE)) + 
  geom_line() + 
  labs(title = paste0(target_name, " (colours = ICBs)"), x = "Age Group", y = "Difference in average monthly rate per 1000 people (Female - male)") + 
  theme_bw() + 
  scale_color_manual(values = ICB_colours, drop = F) + 
  theme(axis.text.y = element_text(size =10)  ,legend.position = "none"
  )

ggsave(paste0("plots/byicb/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_sex_dif.pdf"), plot = PLOT_TEMP2)


}





#### change to look at seasonaility
# average acrosss ICB
ICB_data_2023[, av_across_ICBs := mean(rate_per_1000),by = c("MONTH", "GENDER", "AGE_BAND_NEW", "BNF_CHEMICAL_SUBSTANCE_CODE", "drug_name")]
ICB_data_2023_avICB <- unique(ICB_data_2023[,c("YEAR_MONTH", "ICB_CODE", "ITEMS", "rate_per_1000", "POP") := NULL])
ICB_data_2023_avICB[,av_across_seasons := mean(av_across_ICBs), by =c("BNF_CHEMICAL_SUBSTANCE_CODE", "GENDER","AGE_BAND_NEW", "drug_name" )]
ICB_data_2023_avICB[, seasonal_change := abs(av_across_ICBs/av_across_seasons)]

for(i in drugs_of_interest){
  target <- i
  target_name <- ICB_data_2023_avICB[BNF_CHEMICAL_SUBSTANCE_CODE == target]$drug_name[1]
  ICB_target_2023_avICB <- ICB_data_2023_avICB[BNF_CHEMICAL_SUBSTANCE_CODE == target]

PLOT_TEMP <- ggplot(ICB_target_2023_avICB, aes(x = MONTH, y = av_across_ICBs, colour = AGE_BAND_NEW, group =AGE_BAND_NEW)) + 
  geom_line() + facet_wrap(GENDER~.) + 
  labs(title = paste0(target_name), x = "MONTH", y = "prescription rate per 1000 people, averaged across ICBs") + 
  theme_bw() +
scale_color_manual(values = Age_colours, drop = F) 

ggsave(paste0("plots/byicb/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_seasonality.pdf"), plot = PLOT_TEMP)

PLOT_TEMP2 <- ggplot(ICB_target_2023_avICB, aes(x = MONTH, y = seasonal_change, colour = AGE_BAND_NEW, group =AGE_BAND_NEW)) + 
  geom_line() + facet_wrap(GENDER~.) + 
  labs(title = paste0(target_name), x = "MONTH", y = "monthly proportional change in prescription rate per 1000 people, averaged across ICBs") + 
  theme_bw() +
  scale_color_manual(values = Age_colours, drop = F) + 
  lims(y = c(0,4.6))

ggsave(paste0("plots/byicb/",str_replace_all(target_name, "[^[:alnum:]]", " "), "_seasonality_relative.pdf"), plot = PLOT_TEMP2)

}



#### Look at age distributions by ICB
tot_pop_by_ICB_sex <- dcast.data.table(pop_dat_combo, ICB_2023_Code + ICB_2023_Name + SEX ~ . , value.var = "PEOPLE", fun.aggregate = sum )
colnames(tot_pop_by_ICB_sex)[which(colnames(tot_pop_by_ICB_sex) == ".")] <- "TOT_POP"

pop_dat_combo[tot_pop_by_ICB_sex, on = c("ICB_2023_Code", "ICB_2023_Name", "SEX"), TOT_POP := i.TOT_POP]

pop_dat_combo[, prop_pop := PEOPLE/TOT_POP]

pop_dat_combo[, AGE_BAND_NEW := factor(AGE_BAND_NEW, levels = rev(c("0-1", "2-5", 
                                                                    "6-10", "11-15", "16-20", "21-25", 
                                                                    "26-30", "31-35", "36-40", "41-45", 
                                                                    "46-50", "51-55",  "56-60", "61-65", 
                                                                    "66-70", "71-75", "76-80",  "81-85" ,
                                                                    "86+")))]
pop_dat_combo[, ICB_Name_short := str_replace(ICB_2023_Name, " INTEGRATED CARE BOARD", "")]
pop_dat_combo[, ICB_Name_short := str_replace(ICB_Name_short, "NHS ", "")]


ICB_POPS <- ggplot(pop_dat_combo, aes(x = ICB_Name_short, y = prop_pop, fill = AGE_BAND_NEW)) + 
  facet_grid(GENDER~.) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  scale_fill_manual(values = Age_colours)+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "ICB demographics", x = "ICB", y = "Proportion in each age group")
ICB_POPS
ggsave(paste0("plots/byicb/ICB.pdf"), plot = ICB_POPS, width = 10, height = 10)
