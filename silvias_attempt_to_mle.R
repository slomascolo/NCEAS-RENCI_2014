#Bird traits
traits_commmon_steep <- read.csv("C:/Users/slomascolo/Documents/Silvia/NCEAS-RENCI_2014/BBS_data/commonbirds_steepdecline_traits.csv")
str(traits_commmon_steep)
traits_commmon_steep_insectohio <- as.data.frame(subset(traits_commmon_steep, ohio_presence > 0))
traits_commmon_steep_insectohio$common_name <- factor(toupper(traits_commmon_steep_insectohio$common_name))
levels(traits_commmon_steep_insectohio$common_name)

#Adding AOU codes
AOU_codes <- read.csv("C:/Users/slomascolo/Documents/Silvia/NCEAS-RENCI_2014/BBS_data/AOU_codes.csv")
traits_commmon_steep_insectohio2 <- as.data.frame(merge(AOU_codes, traits_commmon_steep_insectohio))

#BBS data
ohio_BBS <- read.csv("C:/Users/slomascolo/Documents/Silvia/NCEAS-RENCI_2014/BBS_data/fifty7.csv")
ohio_BBS_insect <- as.data.frame(merge(ohio_BBS, traits_commmon_steep_insectohio2))
ohio_BBS_insect$common_name<-factor(ohio_BBS_insect$common_name)
View(ohio_BBS_insect)
ohio_BBS_routes_countiesraw <- read.csv("C:/Users/slomascolo/Documents/Silvia/NCEAS-RENCI_2014/BBS_data/BBSdata.by.county.csv")
head(ohio_BBS_routes_countiesraw)
ohio_BBS_routes_countiesraw2 <- ohio_BBS_routes_countiesraw[!duplicated(ohio_BBS_routes_countiesraw$Route), c(1,4,5,6,7)]
ohio_BBS_birds <- merge(ohio_BBS_insect, ohio_BBS_routes_countiesraw2)
ohio_BBS_birds$route_abundance <- rowSums(ohio_BBS_birds[grep("^Stop[0-9]+", names(ohio_BBS_birds))])
birdsXdiet=aggregate(ohio_BBS_birds$route_abundance, by = list(ohio_BBS_birds$GEOID, ohio_BBS_birds$year, ohio_BBS_birds$diet), sum)
names(birdsXdiet)= c("GEOID", "YEAR", "diet", "abundance")
plot(birdsXdiet$YEAR, birdsXdiet$abundance)
#Geo points
geo = read.csv("C:/Users/slomascolo/Documents/Silvia/NCEAS-RENCI_2014/OH.neonicotinoids.csv", header=TRUE)
neonicXyear = aggregate (geo$density_high, by = list(geo$GEOID, geo$YEAR), sum)
names(neonicXyear)=c("GEOID", "YEAR","density_high")
plot(neonicXyear$YEAR, neonicXyear$density_high)
birdsXgeo = merge (birdsXdiet, neonicXyear, by = c("GEOID", "YEAR"))
birdsXgeo$func.group= ifelse(birdsXgeo$diet=="insects", "insectivore", "other")

#Plot of abundance related to neonics by diet
ins = subset(birdsXgeo, birdsXgeo$diet =="insects")
mam = subset(birdsXgeo, birdsXgeo$diet =="mammals")
omn = subset(birdsXgeo, birdsXgeo$diet =="omnivore")
pla = subset(birdsXgeo, birdsXgeo$diet =="plants")
see = subset(birdsXgeo, birdsXgeo$diet =="seeds")
others= subset(birdsXgeo, birdsXgeo$diet!="insects")

plot(birdsXgeo$density_high, birdsXgeo$abundance, type="n", xlab="", ylab="")
points(ins$density_high, ins$abundance, pch=2, col=3)
points(omn$density_high, omn$abundance, pch=3, col=4)
points(see$density_high, see$abundance, pch=5, col=6)
points(pla$density_high, pla$abundance, pch=4, col=5)
points(mam$density_high, mam$abundance)

#Plotting bird abundance per year
plot(birdsXgeo$YEAR, birdsXgeo$abundance, type="n", xlab="", ylab="")
points(ins$YEAR, ins$abundance, pch=2, col=3)
points(omn$YEAR, omn$abundance, pch=3, col=4)
points(see$YEAR, see$abundance, pch=5, col=6)
points(pla$YEAR, pla$abundance, pch=4, col=5)
points(mam$YEAR, mam$abundance)

#Plotting an exponential curve for a first estimate of parameters
plot(birdsXgeo$density_high, birdsXgeo$abundance, type="n", xlab="", ylab="")
points(ins$density_high, ins$abundance, pch=2, col=3)

x=ins$density_high
tete= function(x, a=700, b=2.75) {a*exp(-b*x)}
plot(tete, add=TRUE)

## The models!
# neg exponential with Poisson distribution
fExpNB=function(a,b){
  lambda=a*(1-exp(-b*ins$density_high))
  -sum(dpois(ins$abundance,lambda,log=T)) 
}

mExpPois= mle2(fExpNB, list(a=600,b=2.75))

summary(mExpPois)
confint(mExpPois)

# neg exponential with neg binom
fExpNB=function(a,b,k){
  media=a*(1-exp(-b*ins$density_high))
  -sum(dnbinom(ins$abundance,mu=media,size=k,log=T)) 
}

mExpNB= mle2(fExpNB, list(a=600,b=2.75,k=4))

summary(mExpNB)
confint(mExpNB)

# linear with neg binom
fLinNB=function(a,b,k){
  media=a+b*ins$density_high
  -sum(dnbinom(ins$abundance, mu=media, size=k, log=T))
}

mLinNB=mle2(fLinNB, list(a=600,b=-10,k=1))

summary(mLinNB)
confint(mLinNB)


# hyperbolic with neg binom
fHypNB=function(a,b,k){
  media=a/(b+ins$density_high)
  -sum(dnbinom(ins$abundance, mu=media, size=k, log=T))
}

mHypNB=mle2(fHypNB, list(a=1100,b=0.3,k=1))

summary(mHypNB)
confint(mHypNB)

#Is it better to separate insectivores from the rest?

#All together
fLinNB_all=function(a,b,k){
  media=a+b*birdsXgeo$density_high
  -sum(dnbinom(birdsXgeo$abundance, mu=media, size=k, log=T))
}

mLinNB_all=mle2(fLinNB_all, list(a=600,b=-10,k=1))

summary(mLinNB_all)
confint(mLinNB_all)

#Others
fLinNB_others=function(a,b,k){
  media=a+b*others$density_high
  -sum(dnbinom(others$abundance, mu=media, size=k, log=T))
}

mLinNB_others=mle2(fLinNB_others, list(a=600,b=-10,k=1))

summary(mLinNB_others)
confint(mLinNB_others)


#within the same model, different parameters for each group... It doesn't work
fLinNB_a=function(aC,aR,b,k){
  a=c(aC,aR)[birdsXgeo$func.group]; media=a + b*x
  -sum(dnbinom(y,mu=media,size=k,log=T)) }
mtA = mle2(fLinNB_a, list(aC=600,aR=600,b=2,k=4), method = "Nelder-Mead", data = list (x = birdsXgeo$density_high, y = birdsXgeo$abundance))

summary(mtA)



AICtab(mLinNB, mExpNB, mExpPois, mHypNB, mLinNB_others, mLinNB_all)



#Plotting the line of the best fit model with the parameters estimated by ML
x=ins$density_high
x1=seq(0,3.5, by=0.05)
LinearNB= function(x, a=95, b=-1.52) {a+b*x}
plot(LinearNB, add=TRUE)

