#Biodiversity/species richness for butterflies (data: Ohio lepidopterans)
btfly = read.csv("/Users/ignaciopazposse/NCEAS-RENCI_2014//butterflies.csv", na.string= "NA")
library(data.table)
Dbtfly = as.data.table(btfly) # otherwise I couldn't do grepl...
Dbtfly = Dbtfly[!grepl("None seen this day",Dbtfly$CommonName),] # leave out surveys with no observations
richXsite = Dbtfly[,length(unique(CommonName)),by="SiteID"] # number of species per site
setnames(richXsite, 2, "sp.rich")
srv.length = aggregate(Dbtfly$duration, by = list(Dbtfly$SiteID), sum, na.rm = TRUE) # total observation time for each site, to be used for standardization per hour 
names(srv.length) = c("SiteID", "srv.time")
btfly.rich = merge(richXsite, srv.length, by = "SiteID")
btfly.rich$sppX1000hrs = btfly.rich$sp.rich/btfly.rich$srv.time*1000 # correcting species richness by effort (total survey time in each site)


#Calculation of diversity (Shannon's index)
div.1 = aggregate(Dbtfly$Total, by = list(Dbtfly$CommonName, Dbtfly$SiteID), sum) #total number of individuals per species observed in each survey
names(div.1) = c("CommonName", "SiteID", "TotalXsp")
totXsite = aggregate(div.1$TotalXsp, by = list(div.1$SiteID), sum) # total number of individuals per site
names(totXsite) = c("SiteID", "TotalXsite")
Dbtfly.div = merge(div.1, totXsite, by = "SiteID")
Dbtfly.div$p.ln.p = -(Dbtfly.div$TotalXsp/Dbtfly.div$TotalXsite)*(log(Dbtfly.div$TotalXsp/Dbtfly.div$TotalXsite))
div.final = aggregate (Dbtfly.div$p.ln.p, by = list(Dbtfly.div$SiteID), sum)
names(div.final) = c("SiteID", "Shannon")
lat = aggregate(Dbtfly$Latitude, by = list(Dbtfly$SiteID), mean)#I just didn't know how to add a column with the latitude for each site...
names(lat) = c("SiteID", "Latitude")
lon = aggregate(Dbtfly$Longitude, by = list(Dbtfly$SiteID), mean)#same as with latitude
names(lon) = c("SiteID", "Longitude")
div.loc=merge(lat, lon)
div.loc.final=merge(div.loc, div.final) 


#Maping points on a map
library(maps) 
library(mapproj)
library(mapdata)
Ohio <- map("state", "ohio")
x=div.loc.final$Longitude
y=div.loc.final$Latitude
locations <- mapproject(x, y) 
points(locations)
