require(ggplot2)
require(reshape2)
require(dplyr)
require(lme4)
require(dismo)
require(grid)

setwd("/Users/Liv/Dropbox/NCEAS/quickstats")
cropprod<-read.csv(file = "census.crops.production.csv", header = T)
# cropprod$Value <- as.numeric(as.character(cropprod$Value))
cropprod$Value <- as.numeric(levels(cropprod$Value))[cropprod$Value]

cropprod$crop_group<-"field_crops" #from NASS database
(grep.position<-grep("OPERATIONS", cropprod$Data.Item))
grep.position<-as.numeric(grep.position)
cropprod<-cropprod[-grep.position,]


head(cropprod)

########Splitting out the data
cropprod.segmented<-cropprod$Data.Item
str(cropprod.segmented)
cropprod.segmented<-as.character(cropprod.segmented)
#cropprod.segmented[903]
cropprod.segmented.split<-strsplit(cropprod.segmented, "-| MEASURED IN ")


cropprod.segmented.split2<-sapply(strsplit(cropprod.segmented, "-| MEASURED IN "), function(str) {paste(rev(str))}) 
#cropprod.segmented.split2

str(cropprod.segmented.split2)
grep.position<-grep("OPERATIONS", cropprod.segmented.split2)
str(grep.position)
grep.position<-as.numeric(grep.position)
cropprod.segmented.split2[grep.position]<-NULL 

##

# # returns string w/o leading whitespace
 trim.leading <- function (x)  sub("^\\s+", "", x)
# 
# # returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
# 
# croprodshortsplit3b<-trim.leading(croprodshortsplit3)
# croprodshortsplit3c<-trim.trailing(croprodshortsplit3b)
length(cropprod.segmented.split2)

head(cropprod)


# cropprod.segmented.split2[[903]][2]


cropprod.segmented.split3<-unlist(lapply(cropprod.segmented.split2,function(x) x[1])) #extracts the first element of the list and makes a vector


cropprod.segmented.split3<-gsub(" ", "", cropprod.segmented.split3)


cropprod$GEOID <- paste(cropprod$State.ANSI, formatC(cropprod$County.ANSI, width = 3, format = "d", flag = "0"), sep = "")

cropprod$yield_unit <- as.factor(cropprod.segmented.split3) 
cropprod$yield_unit.numeric <- ifelse(cropprod$yield_unit == "LB", 1, 
                                      ifelse(cropprod$yield_unit == "GALLONS", 5,
                                             ifelse(cropprod$yield_unit == "BU", 10,
                                                    ifelse(cropprod$yield_unit == "BALES", 50,
                                                           ifelse(cropprod$yield_unit == "CWT", 100, 
                                                                  ifelse(cropprod$yield_unit == "TONS", 1000, ifelse(cropprod$yield_unit == "TONS,DRYBASIS", 2000, "bad_variable")))))))
str(cropprod$yield_unit.numeric)

# from <- c('HAY & HAYLAGE', 'HAYLAGE')
# to <- 'HAY'
# 
# gsub2 <- function(pattern, replacement, x, ...) {
#   for(i in 1:length(pattern))
#     x <- gsub(pattern[i], replacement[i], x, ...)
#   x
# }


cropprod$yield_unit.numeric<-as.numeric(cropprod$yield_unit.numeric)
cropprod$yield_normed <- cropprod$Value * cropprod$yield_unit.numeric
cropprod$Commodity2<-as.character(cropprod$Commodity)
class(cropprod$Commodity2)
cropprod$Commodity3 <- gsub("HAYLAGE", "HAY", cropprod$Commodity2)
cropprod$Commodity3 <- gsub("HAY & HAY", "HAY", cropprod$Commodity3)

cropprod$Commodity3 <- as.factor(cropprod$Commodity3)
cropprod$Commodity3 <- factor(cropprod$Commodity3)
levels(cropprod$Commodity3)
######## plotting splititng by yield_unit
require(dplyr)

cropprod<-cropprod[!is.na(cropprod$Value),]

cropprod_plotting <- cropprod %>%
  group_by(Commodity3) %>%
  summarise(
    yield = mean(yield_normed),
    sd = sd(yield_normed),
    n.t = length(yield_normed),
    se = sd/sqrt(n.t))

head(cropprod_plotting)



#####

cropprod_plotting_sorted <- with(cropprod_plotting,cropprod_plotting[order(-yield),])

ggplot(cropprod_plotting_sorted, aes(y = log(yield), x = reorder(Commodity3, rev(yield))))+
  geom_bar(aes(fill = reorder(Commodity3,rev(yield))), stat = "identity")+
  guides(fill = FALSE)+
  geom_errorbar(aes(ymin = log(yield - se), ymax = log(yield + se)), 
                colour = "black", 
                width = 0.2,
                alpha = 0.7)+
  scale_fill_hue(h = c(100,240), l = 50,c = 100)+
  coord_flip()+
  labs(x = "Commodity", title = "North Carolina: commodities by yield")+
  theme_bw()


