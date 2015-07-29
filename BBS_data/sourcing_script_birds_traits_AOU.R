# This script merges the: 
# trait data,
# AOU codes
# BBS data

# in order for further analysis. 

# Olivia Burge 16 July 2015

require(ggthemes)
require(wesanderson)
require(ggplot2)
require(vegan)
require(dplyr)
require(data.table)
require(tidyr)


# Reading in the new bird data


setwd("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/BBS_data/")
new_bbs <- read.csv("fifty7_withRTENO.csv")# 
new_bbs$sum_route_abundance <- rowSums(new_bbs[grep("^Stop[0-9]+", names(new_bbs))])
head(new_bbs)
names(new_bbs)
new_bbs <- new_bbs[ , c(1:7, 58, 59)] #selects everything except the individual stop abundances
length(unique(new_bbs$Route))
head(new_bbs)
`

# Bird trait data

#Read in and pare down the bird trait data to the scientific, common names for #species and their habitat, nesting, diest, behaviour and conservation status, #and 4&6 letter codes.


bird_traits <- read.csv("OH_BBS_1966-2013_traits.csv")
bird_traits <- bird_traits[ , c(1,2,3,4,5,6,7,8,9)]
head(bird_traits[2])
names(bird_traits) <- toupper(names(bird_traits))
names(bird_traits) <- gsub("[.]", "_", names(bird_traits))  # using the [] to get the period to be used literally. Or can use "fixed = TRUE"
names(bird_traits)
bird_traits$SCIENTIFIC_NAME <- toupper(bird_traits$SCIENTIFIC_NAME)
head(bird_traits)
`

# Bird code AOU data
#BBS data comes with a different kind of AOU code. This chunk tidies up the AOU code dataframe and makes the common and scienitific names compatible with the BBS data. Need to use the common names, scientific names are slightly off in some cases.


AOU_codes <- read.csv("raw_data/AOU_codes.csv") ## a cleaned up (ie header removed, split into columns - no substantive changes to any species names or column headers) version of: ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt

head(AOU_codes)
AOU_codes$common_name <- toupper(AOU_codes$English_Common_Name)
names(AOU_codes)
AOU_codes <- AOU_codes[ , c(2, 10, 6:9)]
names(AOU_codes) <- toupper(names(AOU_codes))
head(AOU_codes)
AOU_codes$SCIENTIFIC_NAME <- paste(AOU_codes$GENUS, AOU_codes$SPECIES, sep = " ")
AOU_codes$SCIENTIFIC_NAME <- toupper(AOU_codes$SCIENTIFIC_NAME)
#testing that all our traits at leat are in the new AOU codes:
bird_traits$SCIENTIFIC_NAME %in% AOU_codes$SCIENTIFIC_NAME # = not all true.
AOU_codes$SCIENTIFIC_NAME %in% bird_traits$SCIENTIFIC_NAME # many many falses
bird_traits$COMMON_NAME <- toupper(bird_traits$COMMON_NAME)
head(bird_traits)
bird_traits$COMMON_NAME %in% AOU_codes$COMMON_NAME # = all true!
AOU_codes$English_Common_Name %in% bird_traits$COMMON_NAME # many many falses

unique(new_bbs$AOU) %in% unique(AOU_codes$AOU) # good.

# merge the AOU codes into the ohio data


# Merging AOU codes and traits
 

head(AOU_codes)
head(new_bbs)

traits_aou <- inner_join(bird_traits, AOU_codes)

BBS_traits <- inner_join(new_bbs, traits_aou)
names(BBS_traits)
`