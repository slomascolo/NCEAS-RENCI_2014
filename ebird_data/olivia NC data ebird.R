#reading ebird data
require(lme4)
require(ggplot2)


test.table<-read.table("/Users/Liv/ebd_US_relMay-2014/ebd_US-NC_relMay-2014/ebd_US-NC_relMay-2014.txt", header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)
str(test.table)
names(test.table)
head(test.table)

test.table$OBSERVATION.DATE <- as.character(test.table$OBSERVATION.DATE)

new.table<-test.table[grep("^2012", test.table$OBSERVATION.DATE), ]

head(new.table)
new.table$OBSERVATION.COUNT<-as.numeric(as.character(new.table$OBSERVATION.COUNT))
summary(new.table$OBSERVATION.COUNT)
length(new.table$OBSERVATION.COUNT)

save(test.table, new.table, file = "/Users/Liv/Dropbox/NCEAS/quickstats/test.tablenew.table.Rdata")
save(new.table, file = "/Users/Liv/Dropbox/NCEAS/quickstats/test.tablenew.only.Rdata")
load(file = "/Users/Liv/Dropbox/NCEAS/quickstats/test.tablenew.table.Rdata")
ebird_sample <- new.table[sample(nrow(new.table), size = 500), ]
save(ebird_sample, file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebird_sample_table.Rdata")
load(file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebird_sample_table.Rdata")

#df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x))
names(ebird_sample)
cols = c(1,4, 8, 9, 10, 19:26, 28:42)
ebird_sample[,cols] = apply(ebird_sample[,cols], 2, function(x) as.character(x))

ebird_sample2 <- ebird_sample[sample(nrow(ebird_sample), size = 40), ]
ebird_sample2 <- droplevels(ebird_sample2)
names(ebird_sample2)
head(ebird_sample2$SUBNATIONAL2_CODE)


require(stringr)
#workflow

str_pad(string, width, side = "left", pad = " ")

ebird_sample2 <-sapply(strsplit(cropprod.segmented, "-| MEASURED IN "), function(str) {paste(rev(str))}) 

save(ebird_sample2, file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebirdsample2.Rdata")
load(file = "/Users/Liv/Dropbox/NCEAS/quickstats/ebirdsample2.Rdata")