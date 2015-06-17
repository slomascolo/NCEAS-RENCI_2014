#Add information to USGS NAWQA Annual Pesticide Use Estimates
#Chemical class from Pestcide Action Network
#Insecticide mode of action from IRAC
library(readr)
library(stringr)
data <- read_csv("C:/Users/Tyson/Desktop/Pesticides/allpesticides1992to2012.csv")
USGSpest <- unique(data$COMPOUND)
USGSpest <- data.frame(cbind(c(1:length(USGSpest)), USGSpest))
names(USGSpest) <- c("Index", "COMPOUND")

setwd("C:/Users/Tyson/Desktop/Pesticides/IRAC")
filenames <- list.files()
MoA <- do.call("rbind", lapply(filenames, read_csv, col_names = FALSE))
names(MoA) <- c("COMPOUND", "MOA")
MoA$COMPOUND <- str_trim(toupper(MoA$COMPOUND), side = "both")
write_csv(MoA, "IRAC_all.csv")


pmatch(USGSpest, MoA$COMPOUND, nomatch = NA, duplicates.ok = FALSE)

test <- merge(USGSpest, MoA, by = "COMPOUND", all.x = TRUE)

write_csv(MoA, "IRAC_all.csv")

#####################################
#Scraping table from HRAC

#DIDNT WORK, either with rvest or XML
#Used Tabula instead

library(dplyr)
library(rvest)

url <- "http://www.hracglobal.com/pages/classificationofherbicidesiteofaction.aspx"

#XPATH of table
###"//*[@id="form1"]/div[6]/fieldset/div/div[3]/center/table"
# load the html code to R
HRAC_html <- url %>% html() 

# filter for the right xpath node
HRAC <- HRAC_html %>% 
  html_nodes(xpath = '//*[@id="form1"]/div[6]/fieldset/div/div[3]/center/table') 

# convert to a data.frame
HRAC<- HRAC %>% html_table() %>% data.frame()


HRAC <- url %>% html() %>% 
  html_nodes(xpath = '//*[@id="form1"]/div[6]/fieldset/div/div[3]/center/table') %>% 
  html_table() %>% data.frame()


library(XML)
test <- readHTMLTable(url, header = TRUE, trim = TRUE, as.data.frame = TRUE, which=1)
