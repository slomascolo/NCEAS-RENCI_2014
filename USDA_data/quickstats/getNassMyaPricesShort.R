# I turned the year and month lists into, well, lists. 
# now it's a lot shorter


years <- 1970:2010
yearList <- list(year=as.character(years))

months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
monthList <- list(reference_period_desc= months)

myParams <- list(source_desc = "SURVEY", sector_desc="CROPS", 
  group_desc="FIELD CROPS", commodity_desc="CORN",
  statisticcat_desc="PRICE RECEIVED", 
  short_desc="CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",  agg_level_desc="NATIONAL",
  state_name = "US TOTAL",
  freq_desc = "MONTHLY",
  reference_period_desc= "JAN",
  breadcrumb = "reference_period_desc")

myParams <- c(myParams, yearList, monthList)

x <- postForm(uri = "http://quickstats.nass.usda.gov/uuid/encode",
              .params = myParams)

x2 <- gsub("\"", "", x)
y  <- getURL(paste("http://quickstats.nass.usda.gov/results/", x2, sep = ""))
yparse <- htmlParse(y, asText=T)
path <- xpathSApply(yparse, "//a", xmlAttrs)[[1]]["href"]
datapath <- gsub(".url", "", strsplit(path, c("/"), fixed = TRUE)[[1]][3])

usdaData <- read.csv(paste("http://quickstats.nass.usda.gov/data/spreadsheet/", datapath, ".csv", sep = ""))


