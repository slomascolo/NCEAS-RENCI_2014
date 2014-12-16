##Experiment with CART for bird functional groups
#STEP 1, Get OH Bird data for one year into Rrrrrrrrrrrrrrr
#That's the sound of a bird zombie getting ready for analysis

getwd()
setwd("/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014/ebird_data")
oh_ebird1 = read.csv("ebird_oh_2000_1yr.csv")
oh_lifehist = read.csv("oh_ebird_lifehist.csv")
#
head(oh_ebird1)
dim (oh_ebird1) #32941    41

head(oh_lifehist)
dim(oh_lifehist) #327   6
oh_birds_traits = merge(oh_ebird1, oh_lifehist, by="SCIENTIFIC.NAME", all.x=TRUE, all.y=TRUE)
head (oh_birds_traits)

write.table(oh_birds_traits, "/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014/ebird_data/oh_birds_traits.csv", sep =",")


##IGNORE
# get spp list OH
#write.table(oh_ebird_df, "/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014/ebird_data/oh_bird_group.csv", sep =",")

#new cols & var names
#oh_ebird1["cl_habitat"] <- NA #new col, place holder
#oh_ebird1["cl_food"] <-NA #new col, place holder 
#oh_ebird1["cl_nest"] <-NA #new col
#oh_ebird1["cl_behav"] <-NA #new col
#oh_ebird1["cl_thrt"] <-NA #new col
#head(oh_ebird1) 

#oh_ebird_df <- data.frame(SCIENTIFIC.NAME=unique(oh_ebird1$SCIENTIFIC.NAME), cl_habitat=rep(NA, length=length(unique(oh_ebird1$SCIENTIFIC.NAME))))
