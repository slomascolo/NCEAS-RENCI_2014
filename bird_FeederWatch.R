
#Bird biodiversity
bird = read.csv("/Users/ignaciopazposse/NCEAS-RENCI_2014/FeederWatch.csv", header = TRUE)
bird.OH = bird[bird$StatProv=="OH",]
length(unique(bird.OH$BirdSpp), by = list(bird.OH$LATITUDE, bird.OH$LONGITUDE))
richXsite.bi = bird.OH[,length(unique(BirdSpp)),by="LATITUDE"] # number of species per site
richXsite = Dbtfly[,length(unique(CommonName)),by="SiteID"] 


#Change in bird diversity across years
#Ohio
bird.OH = as.data.table(bird.OH)
richXyear.OH = bird.OH[,length(unique(BirdSpp)),by="Year"] # number of species per site
setnames(richXyear.OH, 2, "no.spp")
obsXyear.OH = bird.OH[,length(unique(ID)),by="Year"]
setnames(obsXyear.OH, 2, "tot.obs")
richXobs.OH = merge(richXyear.OH, obsXyear.OH, by="Year")
richXobs.OH$richXobs=(richXobs.OH$no.spp/richXobs.OH$tot.obs) 
plot(richXobs.OH$Year, richXobs.OH$richXobs, xlab="Year", ylab="No. spp/observer")

#North Carolina
bird.NC = bird[bird$StatProv=="NC",]
bird.NC = as.data.table(bird.NC)
richXyear.NC = bird.NC[,length(unique(BirdSpp)),by="Year"] # number of species per site
setnames(richXyear.NC, 2, "no.spp") 
obsXyear.NC = bird.NC[,length(unique(ID)),by="Year"]
setnames(obsXyear.NC, 2, "tot.obs")
richXobs.NC = merge(richXyear.NC, obsXyear.NC, by="Year")
richXobs.NC$richXobs=(richXobs.NC$no.spp/richXobs.NC$tot.obs) 
plot(richXobs.NC$Year, richXobs.NC$richXobs, xlab="Year", ylab="No. spp/observer")

#California
bird.CA = bird[bird$StatProv=="CA",]
bird.CA = as.data.table(bird.CA)
richXyear.CA = bird.CA[,length(unique(BirdSpp)),by="Year"] # number of species per site
setnames(richXyear.CA, 2, "no.spp") 
obsXyear.CA = bird.CA[,length(unique(ID)),by="Year"]
setnames(obsXyear.CA, 2, "tot.obs")
richXobs.CA = merge(richXyear.CA, obsXyear.CA, by="Year")
richXobs.CA$richXobs=(richXobs.CA$no.spp/richXobs.CA$tot.obs) 
plot(richXobs.CA$Year, richXobs.CA$richXobs, xlab="Year", ylab="No. spp/observer")

#Michigan
bird.MI = bird[bird$StatProv=="MI",]
bird.MI = as.data.table(bird.MI)
richXyear.MI = bird.MI[,length(unique(BirdSpp)),by="Year"] # number of species per site
setnames(richXyear.MI, 2, "no.spp") 
obsXyear.MI = bird.MI[,length(unique(ID)),by="Year"]
setnames(obsXyear.MI, 2, "tot.obs")
richXobs.MI = merge(richXyear.MI, obsXyear.MI, by="Year")
richXobs.MI$richXobs=(richXobs.MI$no.spp/richXobs.MI$tot.obs) 
plot(richXobs.MI$Year, richXobs.MI$richXobs, xlab="Year", ylab="No. spp/observer")




