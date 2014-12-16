#R CART with BOLKER
library("knitr")
install.packages("rpart", dependencies = TRUE)
install.packages("ade4", dependencies = TRUE)
install.packages("mvpart", dependencies = TRUE)
install.packages("vegan", dependencies = TRUE)
library("vegan")
#library("mvpart") use this LATER...not yet
library("ade4")
library("rpart")

detach("package:mvpart")
summary(cu.summary)
#Grow tree
fit <- rpart(Mileage~Price + Country + Reliability + Type,
             method="anova", xval =100, data=cu.summary))
print(fit)
printcp(fit) #same thing, with complexity parameters
plotcp(fit) #if you get the error that figure margins too large, 
#drag the r-studio window wider (seriously!)
summary(fit) # detailed summary of splits
#This is sort of what I was looking for...not sure about complexity parameters

plot(fit)
text(fit)

#Prune tree to optimal size
best.tree <- which.min(fit$cptable[,"xerror"])
cpval <- fit$cptable[best.tree,"CP"]
pfit <- prune(fit, cp=cpval)

#plot the pruned tree
plot(pfit)
text(pfit)
summary(pfit) #this tree kind of sucks, so need to make better graphics

#pick own tree size
dfit <- rpart(Mileage~., method="anova", 
              maxdepth=2, data=cu.summary)
par(xpd=NA, cex= 0.8) #note that this re-size makes stuff way better
plot(dfit)
text(dfit)
summary(dfit)

#predict stuff: 
dotchart(predict(dfit))

#multivariate example
library(ade4)
data(doubs)
env <- doubs$env   ## site (row) * env variable (column)
spe <- doubs$fish  ## site (row) * species (column)

#transform data
library(vegan)
spe.norm <- as.matrix(decostand(spe, "hellinger"))

library(mvpart)
spe.ch.mvpart<-mvpart(spe.norm ~ ., env,  
                      xv="1se",
                      xval=nrow(spe), 
                      xvmult=100, which=4)
summary(spe.ch.mvpart)
printcp(spe.ch.mvpart)
plotcp(spe.ch.mvpart)

spe.ch.mvpart<-mvpart(data.matrix(spe.norm) ~., env,  
                      xv="pick", xval=nrow(spe), 
                      xvmult=100, which=4)
summary(spe.ch.mvpart)
printcp(spe.ch.mvpart)