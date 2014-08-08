##Experiment with CART for bird functional groups

#STEP 1, Group the Crops into 10 "functional groups"
getwd()
setwd("/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014/USDA_data")
oh_nass = read.csv("ohio_crops_all_nass_2012.csv", head = TRUE)
head(oh_nass)
tail(oh_nass)
dim (oh_nass) #look at the dimensions of raw data from nass

oh_nass["crop_group"] <- NA #new col filled with NA, place holder for crop_group
oh_nass["include_items"] <-NA #new col, place holder for inluded_items
head(oh_nass) #check that col is added

#suggestion from EMMA
#You're going to become friends with strsplit
#commodity_df[7,2] <- "berries"
#df = datatable; in my case oh_nass = datatable

commodity_df <- data.frame(Commodity=unique(oh_nass$Commodity), crop_group=rep(NA, length=length(unique(oh_nass$Commodity))))
head(commodity_df) #check it out
dim(commodity_df) #you'll need the dim for hardcoding in crop_gro