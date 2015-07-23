# creating the "revised_BBS_DATA_foranalysis.csv

new_bbs <- read.csv("BBS_data/fifty7_withRTENO.csv")
new_bbs$sum_route_abundance <- rowSums(new_bbs[grep("^Stop[0-9]+", names(new_bbs))])
new_bbs <- new_bbs[ , c(1:7, 58, 59)]
bird_traits <- read.csv("BBS_data/OH_BBS_1966-2013_traits.csv")
bird_traits <- bird_traits[ , c(1,2,3,4,5,6,7,8,9)]
names(bird_traits) <- toupper(names(bird_traits))
names(bird_traits) <- gsub("[.]", "_", names(bird_traits))
bird_traits$COMMON_NAME <- toupper(bird_traits$COMMON_NAME)
AOU_codes <- read.csv("BBS_data/raw_data/AOU_codes.csv")
AOU_codes$common_name <- toupper(AOU_codes$English_Common_Name)
AOU_codes <- AOU_codes[ , c(2, 10, 6:9)]
names(AOU_codes) <- toupper(names(AOU_codes))
AOU_codes$SCIENTIFIC_NAME <- paste(AOU_codes$GENUS, AOU_codes$SPECIES, sep = " ")
AOU_codes$SCIENTIFIC_NAME <- toupper(AOU_codes$SCIENTIFIC_NAME)
#testing that all our traits at leat are in the new AOU codes:
bird_traits$SCIENTIFIC_NAME %in% AOU_codes$SCIENTIFIC_NAME # =
bird_traits$COMMON_NAME %in% AOU_codes$COMMON_NAME # 
traits_aou <- inner_join(bird_traits, AOU_codes, by = "COMMON_NAME")
head(traits_aou)
BBS_traits <- inner_join(new_bbs, traits_aou)


write.csv(BBS_traits, "BBS_data/revised_BBS_data_for_analysis.csv")