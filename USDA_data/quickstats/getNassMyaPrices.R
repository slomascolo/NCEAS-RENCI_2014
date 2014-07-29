# Thanks to @greghirson who totally knocked this out of the park! 
# Based on his work, here's a simple script to pull monthly prices from the USDA NASS website.

library(RCurl)
library(XML)

x <- postForm(uri = "http://quickstats.nass.usda.gov/uuid/encode",
  source_desc = "SURVEY", sector_desc="CROPS", 
  group_desc="FIELD CROPS", commodity_desc="CORN",
  statisticcat_desc="PRICE RECEIVED", 
  short_desc="CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",  agg_level_desc="NATIONAL",
  state_name = "US TOTAL",
  year = "1977",
  year = "1978",
  year = "1979",
  year = "1980",
  year = "1981",
  year = "1982",
  year = "1983",
  year = "1984",
  year = "1985",
  year = "1986",
  year = "1987",
  year = "1988",
  year = "1989",
  year = "1990",
  year = "1991",
  year = "1992",
  year = "1993",
  year = "1994",
  year = "1995",
  year = "1996",
  year = "1997",
  year = "1998",
  year = "1999",
  year = "2000",
  year = "2001",
  year = "2002",
  year = "2003",
  year = "2004",
  year = "2005",
  year = "2006",
  year = "2007",
  year = "2008",
  year = "2009",
  year = "2010",
  freq_desc = "MONTHLY",
  reference_period_desc= "JAN",
  reference_period_desc= "FEB",
  reference_period_desc= "MAR",
  reference_period_desc= "APR",  
  reference_period_desc= "MAY",
  reference_period_desc= "JUN",
  reference_period_desc= "JUL",
  reference_period_desc= "AUG",  
  reference_period_desc= "SEP",
  reference_period_desc= "OCT",
  reference_period_desc= "NOV",
  reference_period_desc= "DEC",
  breadcrumb = "reference_period_desc")

x2 <- gsub("\"", "", x)
y  <- getURL(paste("http://quickstats.nass.usda.gov/results/", x2, sep = ""))
yparse <- htmlParse(y, asText=T)
path <- xpathSApply(yparse, "//a", xmlAttrs)[[1]]["href"]
datapath <- gsub(".url", "", strsplit(path, c("/"), fixed = TRUE)[[1]][3])

usdaData <- read.csv(paste("http://quickstats.nass.usda.gov/data/spreadsheet/", datapath, ".csv", sep = ""))
