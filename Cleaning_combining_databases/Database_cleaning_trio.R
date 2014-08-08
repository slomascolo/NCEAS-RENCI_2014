#Compendium data cleaning 
load(file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebirdsample2.Rdata") #from olivia NC data ebird.R

#importing fips codes by state
fips_state_index<-read.csv(file = "/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/Cleaning_combining_databases/ALL_USA_FIPS_BY_STATE.csv", header = T)
str(fips_state_index)
head(fips_state_index)

toupper
# Ebird
str(ebird_sample2)
ebird_sample2$state_variable <- toupper(ebird_sample2$STATE_PROVINCE)
head(ebird_sample2)
ebird_sample2$county_variable <- toupper(ebird_sample2$COUNTY)
ebird_sample2$state_id <- gsub(pattern = "([A-Z]\\w)+(\\W)", replacement = "", x = ebird_sample2$state_id)
write.csv(ebird_sample2, file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebirdsample2.txt")
ebird_sample2$state_id2<-ebird_sample2$state_id ## for later
ebird_sample2$state_id<-merge(ebird_sample2["state_id"],fips_state_index[,c("state_initials", "state_fips")], by.x = "state_id", by.y = "state_initials")[,2] #works!

head(ebird_sample2)

ebird_sample2$county_id <-gsub(pattern = "([A-Z]\\w)+(\\W)+([A-Z]\\w)+(\\W)", replacement ="", x = ebird_sample2$SUBNATIONAL2_CODE) #this takes the as.character form of county id, and then finds and deletes the "US-NC-" part, and then converts it to a factor.
class(ebird_sample2$county_id)
head(ebird_sample2$county_id)
### yees

library(stringr)
ebird_sample2$county_id2<-ebird_sample2$county_id # just place holder delete later
ebird_sample2$county_id<-as.numeric(ebird_sample2$county_id)
ebird_sample2$county_id<-str_pad(ebird_sample2$county_id, 3, "left", pad = "0")
head(ebird_sample2$county_id)
# pads out the county ID

ebird_sample2$geoid<-paste(ebird_sample2$state_id,ebird_sample2$county_id, sep = "")

head(ebird_sample2)
ebird_sample2$county_id2<-NULL
ebird_sample2$state_id2<-NULL
names(ebird_sample2)

