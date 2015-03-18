#Bird traits

#setwd("~/RichardHadlee/user1/BurgeO/Documents/GitHub")
setwd("C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data")

traits_commmon_steep <- read.csv("raw_data/commonbirds_steepdecline_traits.csv")
str(traits_commmon_steep)
traits_commmon_steep_insectohio <- as.data.frame(subset(traits_commmon_steep, ohio_presence > 0))
traits_commmon_steep_insectohio$common_name <- factor(toupper(traits_commmon_steep_insectohio$common_name))
levels(traits_commmon_steep_insectohio$common_name)

#Adding AOU codes
AOU_codes <- read.csv("raw_data/AOU_codes.csv")
traits_commmon_steep_insectohio2 <- as.data.frame(merge(AOU_codes, traits_commmon_steep_insectohio))

#BBS data
ohio_BBS <- read.csv("raw_data/fifty7.csv")
ohio_BBS_insect <- as.data.frame(merge(ohio_BBS, traits_commmon_steep_insectohio2))

#rm(ohio_BBS)
ohio_BBS_insect$common_name<-factor(ohio_BBS_insect$common_name)
#View(ohio_BBS_insect)
ohio_BBS_routes_countiesraw <- read.csv("raw_data/BBSdata.by.county.csv")
head(ohio_BBS_routes_countiesraw)
ohio_BBS_routes_countiesraw2 <- ohio_BBS_routes_countiesraw[!duplicated(ohio_BBS_routes_countiesraw$Route), c(1,4,5,6,7)]

########### if you run the following, it shows how many lines you're removing - is this supposd to be the case?
duplicated(ohio_BBS_routes_countiesraw$Route)

ohio_BBS_birds <- merge(ohio_BBS_insect, ohio_BBS_routes_countiesraw2)
head(ohio_BBS_birds)
ohio_BBS_birds$route_abundance <- rowSums(ohio_BBS_birds[grep("^Stop[0-9]+", names(ohio_BBS_birds))])
head(ohio_BBS_birds) # the names thing above doesn't seem to be doing anything? I hope that's not an artefact from me!!

birdsXdiet=aggregate(ohio_BBS_birds$route_abundance, by = list(ohio_BBS_birds$GEOID, ohio_BBS_birds$year, ohio_BBS_birds$diet), sum)
names(birdsXdiet)= c("GEOID", "YEAR", "diet", "abundance")
head(birdsXdiet)

plot(birdsXdiet$YEAR, birdsXdiet$abundance)
#Geo points
geo = read.csv("../OH.neonicotinoids.csv", header=TRUE)
neonicXyear = aggregate (geo$density_high, by = list(geo$GEOID, geo$YEAR), sum)
names(neonicXyear)=c("GEOID", "YEAR","density_high")
plot(neonicXyear$YEAR, neonicXyear$density_high)
birdsXgeo = merge (birdsXdiet, neonicXyear, by = c("GEOID", "YEAR"))
birdsXgeo$func.group= ifelse(birdsXgeo$diet=="insects", "insectivore", "other")
head(birdsXgeo)
#Plot of abundance related to neonics by diet
ins = subset(birdsXgeo, birdsXgeo$diet =="insects")
mam = subset(birdsXgeo, birdsXgeo$diet =="mammals")
omn = subset(birdsXgeo, birdsXgeo$diet =="omnivore")
pla = subset(birdsXgeo, birdsXgeo$diet =="plants")
see = subset(birdsXgeo, birdsXgeo$diet =="seeds")
others= subset(birdsXgeo, birdsXgeo$diet!="insects")

head(ins)

plot(birdsXgeo$density_high, birdsXgeo$abundance, type="n", xlab="Neonics", ylab="Birds")
points(ins$density_high, ins$abundance, pch=2, col=3)
points(omn$density_high, omn$abundance, pch=3, col=4)
points(see$density_high, see$abundance, pch=5, col=6)
points(pla$density_high, pla$abundance, pch=4, col=5)
points(mam$density_high, mam$abundance)

#Plotting bird abundance per year
plot(birdsXgeo$YEAR, birdsXgeo$abundance, type="n", xlab="Year", ylab="Abundance")
points(ins$YEAR, ins$abundance, pch=2, col=3)
points(omn$YEAR, omn$abundance, pch=3, col=4)
points(see$YEAR, see$abundance, pch=5, col=6)
points(pla$YEAR, pla$abundance, pch=4, col=5)
points(mam$YEAR, mam$abundance)

#Plotting an exponential curve for a first estimate of parameters
plot(birdsXgeo$density_high, birdsXgeo$abundance, type="n", xlab="Neonics", ylab="Birds")
points(ins$density_high, ins$abundance, pch=2, col=3)

x=ins$density_high
hyp=function(x, a=10000, b=0.25) {a/(b+x)}
expon= function(x, a=40000, b=4.25) {a*exp(-b*x)}
lin=function(x, a=40000, b=-5000){a+(b*x)}
plot(hyp, add=TRUE)
plot(expon, add=TRUE)
plot(lin, add=TRUE)

## The models!
library(bbmle)
library(emdbook)
# neg exponential with Poisson distribution
fExpPois=function(a,b){
  lambda=a*(1-exp(-b*ins$density_high))
  -sum(dpois(ins$abundance,lambda,log=T)) 
}

########## fails here#############Now I added the library that you need to load so it won't fail
mExpPois= mle2(fExpNB, list(a=20000,b=4.25))

summary(mExpPois)
confint(mExpPois)

# neg exponential with neg binom
fExpNB=function(a,b,k){
  media=a*(exp(-b*ins$density_high))
  -sum(dnbinom(ins$abundance,mu=media,size=k,log=T)) 
}

mExpNB= mle2(fExpNB, list(a=30000,b=4.25,k=4))

summary(mExpNB)
confint(mExpNB)

AICtab(mExpNB, mExpPois)

# linear with neg binom
fLinNB=function(a,b,k){
  media=a+b*ins$density_high
  -sum(dnbinom(ins$abundance, mu=media, size=k, log=T))
}

mLinNB=mle2(fLinNB, list(a=30000,b=-5000,k=1))

summary(mLinNB)
confint(mLinNB)

AICtab(mExpNB, mExpPois, mLinNB)

#Linear Zero-inflated negative binomial
fLinZiNB=function(a,b,k, zprob){
  media=a+b*ins$density_high
  -sum(dzinbinom(ins$abundance, mu=media, size=k, zprob = zprob, log=T))
}

mLinZiNB=mle2(fLinZiNB, list(a=30000,b=-5000,k=1, zprob=0.5))

summary(mLinZiNB)
confint(mLinZiNB)

AICtab(mExpNB, mExpPois, mLinNB, mLinZiNB)


#Exponential zero-inflated negative binomial
fExpZiNB=function(a,b,k, zprob){
  media=a*(exp(-b*ins$density_high))
  -sum(dzinbinom(ins$abundance, mu=media, size=k, zprob = zprob, log=T))
}

mExpZiNB=mle2(fExpZiNB, list(a=40000,b=-50,k=1, zprob=0.5))

summary(mExpZiNB)
confint(mExpZiNB)

AICtab(mExpNB, mExpPois, mLinNB, mLinZiNB, mExpZiNB)
# hyperbolic with neg binom
fHypNB=function(a,b,k){
  media=a/(b+ins$density_high)
  -sum(dnbinom(ins$abundance, mu=media, size=k, log=T))
}

mHypNB=mle2(fHypNB, list(a=10000,b=0.25,k=1))

summary(mHypNB)
confint(mHypNB)

#Is it better to separate insectivores from the rest?

#All together
fLinNB_all=function(a,b,k){
  media=a+b*birdsXgeo$density_high
  -sum(dnbinom(birdsXgeo$abundance, mu=media, size=k, log=T))
}

mLinNB_all=mle2(fLinNB_all, list(a=10000,b=-500,k=1))

summary(mLinNB_all)
confint(mLinNB_all)

#Others
fLinNB_others=function(a,b,k){
  media=a+b*others$density_high
  -sum(dnbinom(others$abundance, mu=media, size=k, log=T))
}

mLinNB_others=mle2(fLinNB_others, list(a=5000,b=-100,k=1))

summary(mLinNB_others)
confint(mLinNB_others)


#within the same model, different parameters for each group... I can't make it work
#fLinNB_a=function(aC,aR,b,k){
#  a=c(aC,aR)[birdsXgeo$func.group]; media=a + b*x
#  -sum(dnbinom(y,mu=media,size=k,log=T)) }
#mtA = mle2(fLinNB_a, list(aC=600,aR=600,b=2,k=4), method = "Nelder-Mead", data = list (x = birdsXgeo$density_high, y = birdsXgeo$abundance))

#summary(mtA)



AICtab(mLinNB, mExpNB, mExpPois, mHypNB)



#Plotting the line of the best fit model with the parameters estimated by ML
plot(birdsXgeo$density_high, birdsXgeo$abundance, type="n", xlab="Neonics", ylab="Birds")
points(ins$density_high, ins$abundance, pch=2, col=3)
x=ins$density_high
x1=seq(0,3.5, by=0.05)
LinearZiNB= function(x, a=29405, b=-2028) {a+b*x}
plot(LinearZiNB, add=TRUE)
ExpZiNB = function(x, a=39973, b=0.2676) {a*(exp(-b*ins$density_high))}
plot(ExpZiNB, add=TRUE)
