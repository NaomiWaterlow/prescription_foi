# looking at the ecdc resistance data

kleb_ceph <- fread("data/ECDC.csv")
kleb_ceph[`Age Group`== "May-18", `Age Group` := "5-18"]

# which of 'our' drugs are 3rd gen cephalosporins? 

unique(all_data_ex$drug_name)
