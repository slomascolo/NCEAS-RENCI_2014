##NMDS  - all 4 states crops

require(vegan)
require(reshape2)
head(cropprod)

cropprod.4states<-cropprod.4states[!is.na(cropprod.4states$Value),]
names(cropprod.4states)
cropprod_species_4states<- cropprod.4states[,c("Commodity3", "yield_normed", "County","State", "GEOID")]

head(subset(cropprod_species_4states, cropprod_species_4states$County=="BEAUFORT"))

cropprod_species_4states2<-dcast(cropprod_species_4states, County + GEOID + State~ Commodity3, value.var = "yield_normed", fun.aggregate = sum)
head(cropprod_species_4states2)
names(cropprod_species_4states2)

cropprod_species_4states2<-cropprod_species_4states2[order(cropprod_species_4states2$State, cropprod_species_4states2$County),]


cropprod_species_4states3<-cropprod_species_4states2[,-c(1:3)]
head(cropprod_species_4states3)
head(cropprod_species_4states2)
rownames(cropprod_species_4states3)<-cropprod_species_4states2[,2]

ohio.nmds<-subset(cropprod_species_4states2, cropprod_species_4states2$State=="OHIO")
michigan.nmds<-subset(cropprod_species_4states2, cropprod_species_4states2$State=="MICHIGAN")
NC.nmds<-subset(cropprod_species_4states2, cropprod_species_4states2$State=="NORTH CAROLINA")
CA.nmds<-subset(cropprod_species_4states2, cropprod_species_4states2$State=="CALIFORNIA")




head(cropprod_species_4states2)
names(cropprod_species_4states2)
crops.NMDS.4states<-metaMDS(cropprod_species_4states3)
crops.NMDS.4states.OH<-metaMDS(ohio.nmds[,4:36])
crops.NMDS.4states.MI<-metaMDS(michigan.nmds[,4:36])
crops.NMDS.4states.NC<-metaMDS(NC.nmds[,4:36])
crops.NMDS.4states.CA<-metaMDS(CA.nmds[,4:36])
plot(crops.NMDS.4states.OH)
plot(crops.NMDS.4states.MI)

crops.NMDS.4states.NC
crops.NMDS.4states.CA


stressplot(crops.NMDS.4states)
plot(crops.NMDS.4states, display = "sites")
plot(crops.NMDS.4states, display = "sp")

plot(crops.NMDS.4states, label = "sites")
summary(crops.NMDS.4states)
cropprod_species_4states2$NMDS1 <- crops.NMDS.4states$points[,1]
cropprod_species_4states2$NMDS2 <- crops.NMDS.4states$points[,2]

species<-data.frame(NMDS1 = crops.NMDS.4states$species[,1], 
                    NMDS2 =crops.NMDS.4states$species[,2])
species$species.names<-rownames(species)

names(cropprod_species_4states2)
crop.nmdsplot.4states<-ggplot(cropprod_species_4states2, aes(x = NMDS1, y = NMDS2))+
  geom_text(aes(label = County))+
  geom_text(data = species, colour = "cornflowerblue", 
            aes(label = species.names, x = NMDS1, y = NMDS2))+
  xlim(-2,2)+
  theme_bw()+
  facet_wrap(~State)
crop.nmdsplot.4states

ggsave(plot = crop.nmdsplot.4states, file = "/Users/Liv/Dropbox/NCEAS/quickstats/crop.nmdsplot1.4states.pdf", width = 210, height = 148, units = "mm", dpi = 600)

levels(cropprod_species_4states2$County)
head(subset(cropprod_species_4states2, cropprod_species_4states2$County=="ALCONA"))

##now for the envfit.
pest.4states<- read.csv("/Users/Liv/Documents/NCEAS_GIT/NCEAS-RENCI_2014/Pesticides/pest.2007.high.csv",header = T)
head(pest.4states)

head(subset(pest.4states, pest.4states$State == "NC" & pest.4states$County.Name=="Beaufort County"))

colnames(pest.4states)[colnames(pest.4states)=="County.Name"] <- "County"
levels(pest.4states$State)<-c("CALIFORNIA", "MICHIGAN", "NORTH CAROLINA", "OHIO" )
levels(cropprod_species_4states2$State)
head(pest.4states)

unique(pest.4states$GEOID)
unique(cropprod_species_4states2$GEOID)
names(pest.4states)

pest.4states$County<-as.character(pest.4states$County)
pest.4states$County<-gsub(" County", "", pest.4states$County)

cropprod_species_4states2$GEOID<-as.character(cropprod_species_4states2$GEOID)
cropprod_species_4states2$GEOID<-gsub(" ", "", cropprod_species_4states2$GEOID)

head(pest.4states[c("GEOID", "State", "County")])
cropprod_species_4states2$County<-toupper(cropprod_species_4states2$County)
pest.4states$County<-toupper(pest.4states$County)
cropprod_species_4states2$County<-as.factor(cropprod_species_4states2$County)
pest.4states$County<-as.factor(pest.4states$County)

cropprod_species_4states2$GEOID<-as.integer(cropprod_species_4states2$GEOID)
class(pest.4states$GEOID)

# pest.4states$YEAR<-as.factor(pest.4states$YEAR) #can't do this as it's only 2007
# levels(pest.4states$YEAR)
# pest.4states.2012<- subset(pest.4states, pest.4states$YEAR==2012, drop = TRUE)
# head(pest.4states.2012)

head(pest.4states)
pest.4states.env<-pest.4states[c("KG", "State", "COMPOUND", "County")]

require(dplyr)

#crop.pest.4states<-left_join(cropprod_species_4states2, pest.4states.env, by = c("County", "State"))
crop.pest.4states<-merge(cropprod_species_4states2, pest.4states.env, by = c("County", "State"))
crop.pest.4states<-NULL
crop.pest.4states<-merge(cropprod_species_4states2, pest.4states.env, by = "GEOID")
##problems


crop.env<-crop.pest.4states[,c("COMPOUND", "County", "State", "KG")]
crop.env.wide<-dcast(crop.env, County + State ~ COMPOUND, value.var = "KG" )
head(crop.env.wide)

crop.env.wide<-crop.env.wide[order(crop.env.wide$State, crop.env.wide$County),]
crop.env.wide[is.na(crop.env.wide)] <- 0
crops.pests.envfit<-envfit(crops.NMDS.4states, env = crop.env.wide, na.rm=TRUE)

dim(crop.env.wide)
dim(cropprod_species_4states2)
names(crop.env.wide)
pest.dists<-vegdist(scale(crop.env.wide[3:345]), "euclid")
species.dists<-vegdist(cropprod_species_4states2[4:36], "bray")
test.mantle<-mantel(pest.dists, species.dists)
