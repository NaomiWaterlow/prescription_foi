# label by AWARE classification
library(data.table)
library(ggplot2)
library(gridExtra)
all_data_ex <- fread("data/all_data_organised.csv")
aware_lookup <- fread("data/AWARE_Classification.csv")

# when name matches add the aware calssification
all_data_ex[aware_lookup, on = c("drug_name"), aware_class := i.Category]

# which ones don't match names? 
missing <- unique(all_data_ex[is.na(aware_class)]$drug_name)

temp <- aware_lookup$drug_name[grepl("Benzylpenicillin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Benzylpenicillin sodium (Penicillin G)", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Phenoxymethylpenicillin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Phenoxymethylpenicillin (Penicillin V)", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Flucloxacillin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Flucloxacillin sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]
all_data_ex[drug_name == "Flucloxacillin magnesium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Amoxicillin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Amoxicillin sodium", aware_class := aware_lookup[drug_name == temp[1], "Category"]]

temp <- aware_lookup$drug_name[grepl("clavulanic", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Co-amoxiclav (Amoxicillin/clavulanic acid)", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Flucloxacillin", aware_lookup$drug_name, fixed = TRUE)]
# Both halves are Access 
all_data_ex[drug_name == "Co-fluampicil(Flucloxacillin/ampicillin)", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Piperacillin", aware_lookup$drug_name, fixed = TRUE)]
temp # second one
all_data_ex[drug_name == "Piperacillin sodium/tazobactam sodium", aware_class := aware_lookup[drug_name == temp[2], "Category"]]

temp <- aware_lookup$drug_name[grepl("Pivmecillinam", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Pivmecillinam hydrochloride", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Cefotaxime", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Cefotaxime sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Ceftriaxone", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Ceftriaxone sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Ceftazidime", aware_lookup$drug_name, fixed = TRUE)]
#dirst one
all_data_ex[drug_name == "Ceftazidime pentahydrate", aware_class := aware_lookup[drug_name == temp[1], "Category"]]

temp <- aware_lookup$drug_name[grepl("Cefuroxime", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Cefuroxime sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Cefuroxime", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Cefuroxime axetil", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Ertapenem", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Ertapenem sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Demeclocycline", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Demeclocycline hydrochloride", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Doxycycline", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Doxycycline hyclate", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Minocycline", aware_lookup$drug_name, fixed = TRUE)]
### there are two. hydroclorine seems to be the oral version. (and wouldn't give IV in primary care)
all_data_ex[drug_name == "Minocycline hydrochloride", aware_class := aware_lookup[drug_name == temp[2], "Category"]]


temp <- aware_lookup$drug_name[grepl("Doxycycline", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Doxycycline monohydrate", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Gentamicin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Gentamicin sulfate", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Neomycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name %in%temp, ]#same - and oral anyway because of community
all_data_ex[drug_name == "Neomycin sulfate", aware_class := aware_lookup[drug_name == temp[2], "Category"]]


temp <- aware_lookup$drug_name[grepl("Erythromycin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Erythromycin ethylsuccinate", aware_class := aware_lookup[drug_name == temp, "Category"]]
all_data_ex[drug_name == "Erythromycin stearate", aware_class := aware_lookup[drug_name == temp, "Category"]]
all_data_ex[drug_name == "Erythromycin lactobionate", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Clindamycin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Clindamycin hydrochloride", aware_class := aware_lookup[drug_name == temp, "Category"]]
all_data_ex[drug_name == "Clindamycin phosphate", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Fosfomycin", aware_lookup$drug_name, fixed = TRUE)]
temp
aware_lookup[drug_name%in%temp,]
# both are oral
all_data_ex[drug_name == "Fosfomycin trometamol", aware_class := aware_lookup[drug_name == temp[2], "Category"]]
all_data_ex[drug_name == "Fosfomycin calcium", aware_class := aware_lookup[drug_name == temp[2], "Category"]]


temp <- aware_lookup$drug_name[grepl("Colistin", aware_lookup$drug_name, fixed = TRUE)]
temp # Oral and IV same classification, but oral as powder
aware_lookup[drug_name%in%temp,]
all_data_ex[drug_name == "Colistin sulfate", aware_class := aware_lookup[drug_name == "Colistin_oral", "Category"]]
 
temp <- aware_lookup$drug_name[grepl("Colisti", aware_lookup$drug_name, fixed = TRUE)]
# this is not absorved orally so needs to be given as an ingection. So colistin-iv
all_data_ex[drug_name == "Colistimethate sodium", aware_class := aware_lookup[drug_name == "Colistin_IV", "Category"]]

temp <- aware_lookup$drug_name[grepl("Fusidic", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Fusidic acid", aware_class := aware_lookup[drug_name == temp, "Category"]]
all_data_ex[drug_name == "Sodium fusidate", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Vancomycin", aware_lookup$drug_name, fixed = TRUE)]
temp # 2 but same classification
all_data_ex[drug_name == "Vancomycin hydrochloride", aware_class := aware_lookup[drug_name == temp[2], "Category"]]

temp <- aware_lookup$drug_name[grepl("Sulfamethoxazole", aware_lookup$drug_name, fixed = TRUE)]
temp# second one
all_data_ex[drug_name == "Co-trimoxazole(Trimethoprim/sulfamethoxazole)", aware_class := aware_lookup[drug_name == temp[2], "Category"]]

temp <- aware_lookup$drug_name[grepl("Rifampicin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Rifampicin combined preparations", aware_class := aware_lookup[drug_name == temp, "Category"]]


temp <- aware_lookup$drug_name[grepl("Metronidazole", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[grepl("Metronidazole", aware_lookup$drug_name, fixed = TRUE),]
#both access
all_data_ex[drug_name == "Metronidazole", aware_class := aware_lookup[drug_name == temp[2], "Category"]]

temp <- aware_lookup$drug_name[grepl("Tinidazole", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name %in% temp,] # both access
all_data_ex[drug_name == "Tinidazole", aware_class := aware_lookup[drug_name == temp[2], "Category"]]


temp <- aware_lookup$drug_name[grepl("Cefox", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name %in% temp,] 
all_data_ex[drug_name == "Cefoxitin sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Temocillin", aware_lookup$drug_name, fixed = TRUE)]
all_data_ex[drug_name == "Temocillin sodium", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Imipenem", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name %in% temp,] 
all_data_ex[drug_name == "Imipenem with cilastatin", aware_class := aware_lookup[drug_name == temp[1], "Category"]]

temp <- aware_lookup$drug_name[grepl("Cefpo", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name %in% temp,] 
all_data_ex[drug_name == "Cefpodoxime", aware_class := aware_lookup[drug_name == temp, "Category"]]

temp <- aware_lookup$drug_name[grepl("Ticar", aware_lookup$drug_name, fixed = TRUE)]
aware_lookup[drug_name %in% temp,] 
all_data_ex[drug_name == "Ticarcillin with clavulanic acid", aware_class := aware_lookup[drug_name == temp, "Category"]]

all_data_ex[drug_name == "Capreomycin", aware_class := "Access"]

missing <- unique(all_data_ex[is.na(aware_class)]$drug_name)

# NOT FOUND
# Dapsone
# Methenamine hippurate
# Nitazoxanide
# cycloserine
# Thalidomide (Antileprotic)
# Aminosalicylic acid
# Taurolidine
# Bedaquiline
# Ethambutol hydrochloride
# Izoniazid
# Pyrazinamide
# Clofazimine


#### ADD the manual ones

# combine into aware categories
all_data_ex_aware <- all_data_ex[!is.na(aware_class)]
all_data_ex_aware <- dcast.data.table(all_data_ex_aware,GENDER + YEAR + MONTH +
                           AGE_BAND + aware_class ~. , value.var = "ITEMS",
                         fun.aggregate = sum)


colnames(all_data_ex_aware)[which(colnames(all_data_ex_aware) == ".")] <- "ITEMS"


# average across months
all_data_ex_aware[, av_across_months := mean(ITEMS),by = c( "GENDER", "AGE_BAND", "aware_class", 'YEAR')]
all_data_ex_aware_av <- unique(all_data_ex_aware[,c( "ITEMS", "MONTH") := NULL])


all_data_ex_aware_av$aware_class <- factor(all_data_ex_aware_av$aware_class, 
                                             levels = c("Access", "Watch", "Reserve"))
all_data_ex_aware_av[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                                         "6-10", "11-15", "16-20", "21-25", 
                                                                         "26-30", "31-35", "36-40", "41-45", 
                                                                         "46-50", "51-55",  "56-60", "61-65", 
                                                                         "66-70", "71-75", "76-80",  "81-85" ,
                                                                         "86+") )]

ggplot(all_data_ex_aware_av, aes(x = AGE_BAND, y = av_across_months,
                                 colour = YEAR, 
                                 group = YEAR)) + 
  geom_line() + 
  facet_grid(GENDER~aware_class) + 
  theme_bw() + 
  labs(y = "average number of prescriptions (across ICBs and months in 2023) per 1000 population",
       title = "AWARE CLASSIFICATION")


tot_prescrips <- all_data_ex_aware_av[, sum(av_across_months), by = c("GENDER", "AGE_BAND", "YEAR")]

all_data_ex_aware_av[tot_prescrips, on=c("GENDER", "AGE_BAND", "YEAR"), total_rate := i.V1]
all_data_ex_aware_av[, perc_each_class := (av_across_months/total_rate)*100]


AWARE1 <- ggplot(all_data_ex_aware_av, aes(x = AGE_BAND, y = perc_each_class,
                                 colour = YEAR, group = YEAR)) + 
  geom_line(stat = "identity") + 
  facet_grid(aware_class~GENDER, scales = "free_y") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(y = "Percent of total prescriptions by age group in each category",
       title = "AWaRe Classifications", x = "Age band", colour = "Year") + 
  geom_hline(data =all_data_ex_aware_av[aware_class=="Access"], aes(yintercept = 60), 
             linetype = "dashed") + 
  geom_hline(data =all_data_ex_aware_av[aware_class=="Access"], aes(yintercept = 80), 
             linetype = "dashed") + 
  geom_vline(xintercept = "11-15", linetype = "dotted")+ 
  geom_vline(xintercept = "16-20", linetype = "dotted")

# which are the drugs that cause the big uptick in young men in watch? 
drug_specific <- all_data_ex[!is.na(aware_class),sum(ITEMS), by = c("YEAR", "GENDER", "AGE_BAND", "drug_name", "aware_class")]

drug_specific[, AGE_BAND := factor(AGE_BAND, levels = c("0-1", "2-5", 
                                                               "6-10", "11-15", "16-20", "21-25", 
                                                               "26-30", "31-35", "36-40", "41-45", 
                                                               "46-50", "51-55",  "56-60", "61-65", 
                                                               "66-70", "71-75", "76-80",  "81-85" ,
                                                               "86+") )]
drug_specific$aware_class <- factor(drug_specific$aware_class, 
                                           levels = c("Access", "Watch", "Reserve"))


AWARE2 <- ggplot(drug_specific[YEAR == 2023 ], aes(x = AGE_BAND, y = V1, group = drug_name)) + 
  facet_grid(aware_class~GENDER, scale = "free_y") + 
  geom_line() + 
  geom_line(data = drug_specific[YEAR==2023 & (drug_name == "Lymecycline" | drug_name == "Colistimethate sodium" |
                                                 drug_name == "Amoxicillin" | drug_name == "Clarithromycin")], 
            aes(x = AGE_BAND, y = V1, colour = str_wrap(drug_name, 10), group = drug_name), linewidth = 1) + 
  theme_bw() + 
  labs(x = "Age band", y = "Total prescriptions", title = "AWaRe Classifications (2023), with key drugs highlighted", 
       colour = "Drug") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(xintercept = "11-15", linetype = "dotted")+ 
  geom_vline(xintercept = "16-20", linetype = "dotted")


FIG3 <- grid.arrange(AWARE1, AWARE2, ncol=2)


ggsave(paste0("plots/Fig3.pdf"), plot = FIG3, 
       width = 20, height = 10)
# calculate the total % in aware across alla ge groups and sex
overall_aware <- drug_specific[YEAR == 2023, sum(V1), by = c("aware_class")]
overall_aware[, total := sum(overall_aware$V1)]
overall_aware[, proportion := V1/total]
