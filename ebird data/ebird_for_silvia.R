ebird_ohio<-read.table("/Users/Liv/ebd_US_relMay-2014/ebd_US-OH_relMay-2014/ebd_US-OH_relMay-2014.txt", header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)

ebird_ohio$OBSERVATION.COUNT = as.numeric(ebird_ohio$OBSERVATION.COUNT)
ebird_ohio$LATITUDE = as.numeric(ebird_ohio$LATITUDE)
ebird_ohio$LONGITUDE = as.numeric(ebird_ohio$LONGITUDE)
ebird_ohio$DURATION.MINUTES = as.numeric(ebird_ohio$DURATION.MINUTES)
ebird_ohio$NUMBER.OBSERVERS = as.numeric(ebird_ohio$NUMBER.OBSERVERS)
ebird_ohio$OBSERVATION.YEAR <- as.character(ebird_ohio$OBSERVATION.DATE)

ebird_ohio_twothousand<-ebird_ohio[grep("^20", ebird_ohio$OBSERVATION.YEAR), c(ebird_ohio$STATE_PROVINCE, ebird_ohio$SUBNATIONAL1_CODE, ebird_ohio$OBSERVATION.DATE, ebird_ohio$OBSERVATION.YEARebird_ohio$SUBNATIONAL2_CODE, ebird_ohio$COMMON.NAME, ebird_ohio$SCIENTIFIC.NAME, ebird_ohio$OBSERVATION.COUNT, ebird_ohio$COUNTY, ebird_ohio$LATITUDE, ebird_ohio$LONGITUDE, ebird_ohio$DURATION.MINUTES, ebird_ohio$NUMBER.OBSERVERS)]


