setwd("C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data")

traits_common_steep <- read.csv("raw_data/commonbirds_steepdecline_traits.csv")
str(traits_common_steep)
traits_common_steep_ohio <- as.data.frame(subset(traits_common_steep, ohio_presence > 0))
traits_common_steep_ohio$common_name <- factor(toupper(traits_common_steep_ohio$common_name))
levels(traits_common_steep_ohio$common_name)

#Adding AOU codes
AOU_codes <- read.csv("raw_data/AOU_codes.csv")[,c(2,3)]
AOU_codes$English_Common_Name <- factor(toupper(AOU_codes$English_Common_Name))
levels(AOU_codes$English_Common_Name)

traits_common_steep_ohio2 <- merge(traits_common_steep_ohio, AOU_codes, by.x = "common_name", by.y = "English_Common_Name", all=FALSE)

#BBS data
ohio_BBS <- read.csv("fifty7_withRTENO.csv")
ohio_BBS_steep <- merge(ohio_BBS, traits_common_steep_ohio2, by.x="AOU", by.y="AOU")

#rm(ohio_BBS)
#ohio_BBS_insect$common_name<-factor(ohio_BBS_insect$common_name)
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
birdsXgeo1997 = birdsXgeo[-c(birdsXgeo$YEAR=="1996"),]

  #Plot of abundance related to neonics by diet
ins = subset(birdsXgeo1997, birdsXgeo1997$diet =="insects")
mam = subset(birdsXgeo1997, birdsXgeo1997$diet =="mammals")
omn = subset(birdsXgeo1997, birdsXgeo1997$diet =="omnivore")
pla = subset(birdsXgeo1997, birdsXgeo1997$diet =="plants")
see = subset(birdsXgeo1997, birdsXgeo1997$diet =="seeds")
others= subset(birdsXgeo1997, birdsXgeo1997$diet!="insects")

y = log(ins$abundance + 1)
n = length(ins$abundance)
x = (ins$density_high-mean(ins$density_high))/sd(ins$density_high) #centered x, to see if it helps running the model

#coding GeoIDs

geoid = as.vector(ins$GEOID)
uniq.name = unique(geoid)
J = length(uniq.name)
geo = rep(NA, J)
for (i in 1:J){
  geo[geoid==uniq.name[i]] <-i
}


#Pooled regression
lm.pooled = lm (y~x)
summary(lm.pooled)

#Un-pooled regression, each county has its own slope, I think
lm.unpooled.0 = lm(formula = y ~ x + factor(geo))
summary(lm.unpooled.0)

#Here each county has its own intercept
lm.unpooled = lm(formula = y ~ x + factor(geo)-1)
summary(lm.unpooled)

#The model, varying intercept per GEOID
library(R2WinBUGS)
bird.data = list("n", "J", "y", "geo", "x")
bird.inits <-function(){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm (1),
        sigma.y=runif(1), sigma.a=runif(1))}
bird.parameters = c("a", "b", "mu.a", "sigma.y", "sigma.a")
bird.1 = bugs(bird.data, bird.inits, bird.parameters, bugs.directory="C:/Program Files (x86)/WinBUGS14",
              "C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data/bird.1.bug", n.chains=3, n.iter=1000, debug=TRUE)
plot(bird.1)
print(bird.1)

#The model, varying intercept and slope per GEOID using Wishart distribution
W <- diag (2)
birdW.data <- list("n", "J", "y", "geo", "x", "W")
birdW.inits <- function(){
  list (B.raw=array(rnorm(2*J), c(J,2)), mu.a.raw=rnorm(1),
        mu.b.raw=rnorm(1), sigma.y=runif(1), Tau.B.raw=rWishart(df=3,n=length(ins$abundance), Sigma=diag(2)),
        xi.a=runif(1), xi.b=runif(1))}
birdW.parameters <- c("a", "b", "mu.a", "mu.b", "sigma.y", "sigma.a", "sigma.b", "rho")
birdW <- bugs (birdW.data, birdW.inits, birdW.parameters,  bugs.directory="C:/Program Files (x86)/WinBUGS14",
           "C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data/hierarchical_model/wishart1.bug", n.chains=3, n.iter=1000, debug=TRUE)

plot(birdW)
print(birdW)


  
#Summarizing classical and multilevel inferences graphically

display8 = c (36, 1, 35, 21, 14, 71, 61, 70)#choose 8 counties
x.jitter = x + runif (n, -.05, .05)
x.range = range ( x.jitter)
y.range = range (y[!is.na(match(county, display8))])

a.pooled = coef(lm.pooled)[1]
b.pooled = coef(lm.pooled)[2]
a.nopooled = coef(lm.unpooled)[2:(J+1)]
b.nopooled = coef(lm.unpooled[1])

#summarize the parameters in the fitted multilevel model by their mean estimates
attach.bugs(radon.1)
a.multilevel = rep(NA, J)
for (j in 1:J){
  a.multilevel[j] <- median (a[,j])
}
b.multilevel = median (b)

#make graphs
par (mfrow=c(2,4))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05, 1.05),
        ylim=y.range, xlab="floor", ylab="log radon level")
  curve (a.pooled + b.pooled*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (a.nopooled[j] + b.nopooled*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.multilevel[j] + b.multilevel*x, lwd=1, col="black", add=TRUE)
}
sample.size <- as.vector (table (county))
sample.size.jitter <- sample.size*exp(runif(J, -.1,.1))
plot (sample.size.jitter, a.multilevel, xlab="sample size in county j", 
      ylim=range(y), ylab=expression (paste ("intercept, ", alpha[j],
                                             " (multilevel inference)")), pch=20, log="x")
for (j in 1:J){
  lines (rep(sample.size.jitter[j],2), median(a[,j])+c(-1,1)*sd(a[,j]))
}
abline (a.pooled, 0, lwd=.5)
