dd <- ins #read.table("insectivores_hier.csv",header=TRUE)

library("ggplot2"); theme_set(theme_bw())
ggplot(dd,aes(density_high,abundance,colour=YEAR))+geom_point()+
    scale_y_log10()+scale_x_log10()+
        geom_line(aes(group=GEOID),colour="gray",alpha=0.3)
## increase in density_high (pesticide) over time but otherwise
## no obvious pattern; no obvious spatial relationship

ggplot(dd,aes(YEAR,abundance,colour=density_high))+geom_point()+
    scale_y_log10()+scale_x_log10()+
        geom_line(aes(group=GEOID),colour="gray",alpha=0.3)


ggplot(dd,aes(YEAR,density_high,colour=abundance))+geom_point()+
    scale_y_log10()+
        geom_line(aes(group=GEOID),colour="gray",alpha=0.3)

## more among-GEOID variation in pesticide use than in abundance
## noticeable jump in pesticide use 2003-2004

library(R2jags)
## J; number of geoIDs
## n; number of bird counts
## y; log(abundance)  (n)
## geo; geoID for a given y (n)
## x; neonic application

bird.data <- with(dd,list(n=nrow(dd),
                          J=length(unique(GEOID)),
                          y=abundance,
                          x=density_high,
                          geo=as.numeric(factor(GEOID))))

J <- length(unique(dd$GEOID)) ## hack
bird.inits <-function(){
    list (a=rnorm(J), b=rnorm(1), mu.a=rnorm (1),
          sigma.y=runif(1), sigma.a=runif(1))
}
bird.parameters = c("a", "b", "mu.a", "sigma.y", "sigma.a")

library("R2jags")                  
bird.1 <- jags(data=bird.data, inits=bird.inits,
               parameters.to.save=bird.parameters,
               model.file="bird.1.bug",
               n.chains=3,n.iter=1000)

##bugs.directory="C:/Program Files (x86)/WinBUGS14",
##              "C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data/bird.1.bug", n.chains=3, n.iter=1000, debug=TRUE)

## depending on how interested you are in the among-GEOID variation,
## it might be useful to parameterize the model as (overall mean+GEOID deviation)
## so that you could examine the *average* intercept estimate more
## easily

plot(bird.1)
print(bird.1)

## possibly more useful plots:

library(plotMCMC)  ## pretty but doesn't do multiple chains
library(coda)
library(emdbook)
library(lattice)
m1 <- as.mcmc.bugs(bird.1$BUGSoutput)
summary(m1[,-(1:51)])

mn <- colnames(m1[[1]])
## exclude variables starting with "a[", and deviance
densityplot(m1[,!grepl("(a\\[|deviance)",mn)])
xyplot(m1[,!grepl("(a\\[|deviance)",mn)])

## from plotMCMC
plotTrace(lump.mcmc.list(m1[,-(1:51)]))
plotDens(lump.mcmc.list(m1[,-(1:51)]))

## might be easier to 
library("lme4")
m2 <- lmer(abundance~density_high+(1|GEOID),data=dd)

library("MCMCglmm")
m3 <- MCMCglmm(abundance~density_high,
         random=~GEOID,data=dd)

##The model, varying intercept and slope per RTENO using Wishart distribution
W <- diag (2)
birdW.data <- c(bird.data,list(W=W))
birdW.inits <- function(){
    list (B.raw=array(rnorm(2*J), c(J,2)), mu.a.raw=rnorm(1),
          mu.b.raw=rnorm(1), sigma.y=runif(1),
          Tau.B.raw=drop(rWishart(n=1, df=3,Sigma=diag(2))),
          xi.a=runif(1), xi.b=runif(1))}
birdW.parameters <- c("a", "b", "mu.a",
                      "mu.b", "sigma.y", "sigma.a", "sigma.b", "rho")
birdW <- jags (birdW.data, birdW.inits, birdW.parameters,
               model.file="hierarchical_model/wishart1.bug",
               n.chains=3, n.iter=100000)
 ## bugs.directory="C:/Program Files (x86)/WinBUGS14",
 ##               "C:/Users/silvia/Documents/NCEAS-RENCI_2014/BBS_data/hierarchical_model/wishart1.bug", 

plot(birdW)
print(birdW)

birdW2 <- lmer(abundance~density_high+(density_high|GEOID),data=dd)

dd1 <- dd
library("MCMCglmm")
m3 <- MCMCglmm(abundance~density_high,
               random=~us(1+density_high):GEOID,data=dd1,
               prior=list(R=list(V=1,nu=0.002),
                          G=list(G1=list(V=diag(2),nu=3))))
