#reading ebird data
install.packages("spocc", dependencies = TRUE)
require(spocc)

test.table<-read.table("/Users/Liv/ebd_US_relMay-2014/ebd_US-NC_relMay-2014/ebd_US-NC_relMay-2014.txt", header = TRUE, fill = TRUE, sep = "\t")
names(test.table)
head(test.table)

test.table$OBSERVATION.DATE <- as.character(test.table$OBSERVATION.DATE)

new.table<-test.table[grep("^2012", test.table$OBSERVATION.DATE), ]

head(new.table)
new.table$OBSERVATION.COUNT<-as.numeric(as.character(new.table$OBSERVATION.COUNT))
summary(new.table$OBSERVATION.COUNT)
length(new.table$OBSERVATION.COUNT)

save(test.table, new.table, file = "/Users/Liv/Dropbox/NCEAS/quickstats/test.tablenew.table.Rdata")