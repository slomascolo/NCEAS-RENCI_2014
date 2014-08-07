bird.oh = read.csv("/Users/ignaciopazposse/NCEAS-RENCI_2014/BBS_data/Ohio_BBS.csv", header = TRUE)
bird.oh2000=bird.oh[grep("20", bird.oh$Year),]
routes = read.csv("/Users/ignaciopazposse/NCEAS-RENCI_2014/BBS_data/routes.BBS.csv", header = TRUE)
routes.oh = routes[grep("66", routes$statenum),]
bird.oh.div= aggregate(bird.oh2000$SpeciesTotal, by = list(bird.oh2000$Route, bird.oh2000$Year), sum)
names(bird.oh.div) = c("Route", "Year", "countXrouteXyear")
oh.div=merge(bird.oh2000, bird.oh.div, by = c("Route", "Year"))
oh.div$p.ln.p = (oh.div$SpeciesTotal/oh.div$countXrouteXyear)*log(oh.div$SpeciesTotal/oh.div$countXrouteXyear)
oh.div.final = aggregate (oh.div$p.ln.p, by = list(oh.div$Route, oh.div$Year), sum)
names(oh.div.final) = c("Route", "Year", "Shannon")
oh.div.final$Shannon = (oh.div.final$Shannon)*(-1)
routes.oh.loc=routes.oh[,c(3,5,6)]
bird.div.loc = merge(oh.div.final, routes.oh, by = "Route")[,c(1:3,7,8)]
write.table(bird.div.loc, "/Users/ignaciopazposse/NCEAS-RENCI_2014/BBS_data/BirdDiv_BBS_OH.csv", sep=",")

##Diversity by rough functional groups
bird.oh2000.FG = bird.oh2000[bird.oh2000$Aou==c("7210","6821","7550","6080","5420","4940","5011","3870","6330","4660"),]
