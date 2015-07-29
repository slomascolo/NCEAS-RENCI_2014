setwd("C:/Users/silvia/Dropbox/modelo_jerarquico")
dd <- read.table("datos_insectivoros.csv",header=TRUE)

##La pregunta del trabajo es: cómo afecta la aplicación de pesticidas a la abundancia de aves cercanas a lotes de agricultura?
##los datos de abundancia de aves fueron tomados en transectas fijas anho tras anho. 
##Por esto me interesa controlar la variabilidad por transecta y por anho (y eventualmente por dieta del ave)


library(jagsUI)
## J; number of transects where bird abundance was recorded (GEOID)
## n; number of bird counts
## y; log(abundance)  (n) of birds
## geo; geoID for a given y (n) 
## x; amount of pesticide applied

bird.data <- with(dd,list(n=nrow(dd),
                          J=length(unique(GEOID)),
                          Z=length(unique(YEAR)),
                          y=as.numeric(abundance),
                          x=density_high,
                          geo=as.numeric(factor(GEOID)),
                          year=as.numeric(factor(YEAR))))

cat(file="sil.bug",
    "model {
    for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[geo[i],year[i]] + b[geo[i],year[i]]*x[i]
    }
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif(0, 1000)
    
    for (z in 1:Z){
    mu.a[z] ~ dnorm(my.a, tauy.a)
    mu.b[z] ~ dnorm(my.b, tauy.b)
    for(j in 1:J){
    a[j,z] ~ dnorm(mu.a[z], tau.a)
    b[j,z] ~ dnorm(mu.b[z], tau.b)

    }
    }
    
    my.a ~ dnorm (0, .0001)
    my.b ~ dnorm (0, .0001)
    
    s.a ~ dunif (0, 100)
    s.b ~ dunif (0, 100)
    sy.a ~ dunif (0, 100)
    sy.b ~ dunif (0, 100)
    tauy.a <- pow(sy.a, -2)
    tauy.b <- pow(sy.b, -2) 
    tau.a <- pow(s.a, -2)
    tau.b <- pow(s.b, -2) 
    
    }")



datos <- c(bird.data)

J <- bird.data$J
Z <- bird.data$Z
y <- bird.data$y
bird.inits <- function(){
  list (a=array(rnorm(J*Z, mean(y),0.1), c(J,Z)), 
  b=array(rnorm(J*Z, 0,0.001), c(J,Z)),
  sigma.y=105)
}
bird.parameters <- c("mu.a", "mu.b", "my.a", "my.b", "s.a", "s.b", "sy.a", "sy.b")
bird.sim <- jags (datos, inits=bird.inits, bird.parameters,
               model.file="sil.bug",
               n.chains=3, n.iter=100000, n.burnin=5000, n.thin=3)
