#practice with OH ag data from NASS and renaming/creating variables
#original data is "ohio_crops_all_nass_2012.csv" and is posted to GitHub
#last update by KG 7-31-14

getwd()
setwd("/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014/USDA_data")
oh_nass = read.csv("ohio_crops_all_nass_2012.csv", head = TRUE)
head(oh_nass)
tail(oh_nass)
dim (oh_nass) #look at the dimensions of raw data from nass
#attach(oh_nass) #attach raw data from nass

oh_nass["crop_group"] <- NA #new col filled with NA, place holder for crop_group
oh_nass["include_items"] <-NA #new col, place holder for inluded_items
head(oh_nass) #check that col is added

#suggestion from EMMA
#You're going to become friends with strsplit
#commodity_df[7,2] <- "berries"
#df = datatable; in my case oh_nass = datatable

commodity_df <- data.frame(Commodity=unique(oh_nass$Commodity), crop_group=rep(NA, length=length(unique(oh_nass$Commodity))))
head(commodity_df) #check it out= WIN!
dim(commodity_df) #you'll need the dim for hardcoding in crop_group

# from here you can 1) save to csv and add manually outside of R (in excel)
# 2) hardcode in R
# this is an option for hardcoing in R
#dataframe[row,col] <-"assign this thing to the position specified"
commodity_df[1,2] <- "fruits_nuts"
commodity_df[2,2] <- "fieldcrops"
commodity_df[3,2] <- "fieldcrops"
commodity_df[4,2] <- "other"
commodity_df[5,2] <- "veg_melon"
commodity_df[6,] #emma's trick is to hit "command enter" this runs just this line, 
#trick also works with running multiple rows as a subset; highlight & "command enter"
commodity_df[6,2] <- "berries"
commodity_df[7,2] <- "berries"
commodity_df[8,2] <- "berries"
commodity_df[9,2] <- "veg_melon"
commodity_df[10,2] <- "other"
commodity_df[11,2] <- "veg_melon"
commodity_df[12,2] <- "veg_melon"
commodity_df[13,2] <- "fruits_nuts"
commodity_df[14,2] <- "fieldcrops"
commodity_df[15,2] <- "totals"
commodity_df[16,2] <- "veg_melon"
commodity_df[17,2] <- "xmas_trees"
commodity_df[18,2] <- "xmas_trees"
commodity_df[19,2] <- "other"
commodity_df[20,2] <- "fieldcrops"
commodity_df[21,2] <- "other"
commodity_df[22,2] <- "totals"
commodity_df[23,2] <- "other"
commodity_df[24,2] <- "other"
commodity_df[25,2] <- "totals"
commodity_df[26,2] <- "totals"
commodity_df[27,2] <- "veg_melon"
commodity_df[28,2] <- "fieldcrops"
commodity_df[29,2] <- "fruits_nuts"
commodity_df[30,2] <- "veg_melon"
commodity_df[31,2] <- "seeds_hay"
commodity_df[32,2] <- "seeds_hay"
commodity_df[33,2] <- "seeds_hay"
commodity_df[34,2] <- "totals"
commodity_df[35,2] <- "other"
commodity_df[36,2] <- "veg_melon"
commodity_df[37,2] <- "totals"
commodity_df[38,2] <- "totals"
commodity_df[39,2] <- "fieldcrops"
commodity_df[40,2] <- "veg_melon"
commodity_df[41,2] <- "veg_melon"
commodity_df[42,2] <- "fruits_nuts"
commodity_df[43,2] <- "other"
commodity_df[44,2] <- "fruits_nuts"
commodity_df[45,2] <- "fruits_nuts"
commodity_df[46,2] <- "veg_melon"
commodity_df[47,2] <- "veg_melon"
commodity_df[48,2] <- "fruits_nuts"
commodity_df[49,2] <- "other"
commodity_df[50,2] <- "other"
commodity_df[51,2] <- "veg_melon"
commodity_df[52,2] <- "berries"
commodity_df[53,2] <- "fieldcrops"
commodity_df[54,2] <- "other"
commodity_df[55,2] <- "other"
commodity_df[56,2] <- "fieldcrops"
commodity_df[57,2] <- "fieldcrops"
commodity_df[58,2] <- "veg_melon"
commodity_df[59,2] <- "berries"
commodity_df[60,2] <- "fielcrops"
commodity_df[61,2] <- "other"
commodity_df[62,2] <- "veg_melon"
commodity_df[63,2] <- "fruits_nuts"
commodity_df[64,2] <- "seeds_hay"
commodity_df[65,2] <- "totals"
commodity_df[66,2] <- "veg_melon"
commodity_df[67,2] <- "fruits_nuts"
commodity_df[68,2] <- "fieldcrops"
commodity_df[69,2] <- "fieldcrop"
commodity_df[70,2] <- "veg_melon"
commodity_df[71,2] <- "fruits_nuts"
commodity_df[72,2] <- "other"
commodity_df[73,2] <- "other"
commodity_df[74,2] <- "seeds_hay"
commodity_df[75,2] <- "veg_melon"
commodity_df[76,2] <- "veg_melon"
commodity_df[77,2] <- "veg_melon"
commodity_df[78,2] <- "berries"
commodity_df[79,2] <- "veg_melon"
commodity_df[80,2] <- "fieldcrops"
commodity_df[81,2] <- "fruits_nuts"
commodity_df[82,2] <- "fruits_nuts"
commodity_df[83,2] <- "fruits_nuts"
commodity_df[84,2] <- "other"
commodity_df[85,2] <- "veg_melon"
commodity_df[86,2] <- "other"
commodity_df[87,2] <- "other"
commodity_df[88,2] <- "other"
commodity_df[89,2] <- "other"
commodity_df[90,2] <- "other"
commodity_df[91,2] <- "other"
commodity_df[92,2] <- "other"
commodity_df[93,2] <- "veg_melon"
commodity_df[94,2] <- "veg_melon"
commodity_df[95,2] <- "veg_melon"
commodity_df[96,2] <- "fruits_nuts"
commodity_df[97,2] <- "fieldcrops"
commodity_df[98,2] <- "fieldcrops"
commodity_df[99,2] <- "other"
commodity_df[100,2] <- "veg_melon"
commodity_df[101,2] <- "fruits_nuts"
commodity_df[102,2] <- "fruits_nuts"
commodity_df[103,2] <- "fruits_nuts"
commodity_df[104,2] <- "veg_melon"
commodity_df[105,2] <- "fruits_nuts"
commodity_df[106,2] <- "seeds_hay"
commodity_df[107,2] <- "fruits_nuts"
commodity_df[108,2] <- "seeds_hay"
commodity_df[109,2] <- "other"
commodity_df[110,2] <- "veg_melon"
commodity_df[111,2] <- "veg_melon"
commodity_df[112,2] <- "other"