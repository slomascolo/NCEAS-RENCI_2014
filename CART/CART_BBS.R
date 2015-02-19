
getwd()
setwd("/Users/kellyg/git_oss_project_2014/NCEAS-RENCI_2014")
oh_bbs = read.csv("OH_BBS_1966-2013_kg.csv", header = TRUE)
head (oh_bbs)
library(rpart)

Y <- as.matrix(oh_bbs[,11:57])
Y1 <- cbind(oh_bbs$X1966, oh_bbs[,12])
is.matrix(Y)
fitbirds <- rpart(Y~ habitat + nesting + behavior + conservation,  method="anova", maxdepth=4, data=oh_bbs)


fitbirds <- rpart(total_2000~ habitat + nesting + behavior + conservation,  method="anova", maxdepth=4, data=oh_bbs)

printcp(fitbirds)
par(xpd=NA,cex=0.8)
plot(fitbirds)
text(fitbirds)

fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis) # from example(rpart)
par(mfrow=c(1,2), xpd=NA) # side by side comparison
plot(fit)
text(fit, use.n=TRUE)
library("rpart.plot")
prp(fit, extra=1, uniform=F, branch=1, yesno=F, border.col=0, xsep="/")


summary(oh_bbs$diet)
tapply(oh_bbs$total_1995, list(oh_bbs$diet,oh_bbs$habitat), sum)
model1<- glm(total_1995~habitat+diet+nesting+behavior, data=oh_bbs, family="poisson")
summary(model1)

####
bird.oh = read.csv("/Users/k/NCEAS-RENCI_2014/BBS_data/Ohio_BBS.csv", header = TRUE)
spp = read.table("/Users/ignaciopazposse/NCEAS-RENCI_2014/BBS_data/SpeciesList.txt", header=TRUE, sep="\t")
func.groups=read.csv("/Users/ignaciopazposse/NCEAS-RENCI_2014/BBS_data/oh_ebird_lifehist.csv")
setnames(func.groups, 1, "Aou")
func.groups[11,1]="0070"
div.group= merge(oh.div, func.groups, by = "Aou", all=FALSE)
str(div.group)
unique(div.group$Aou)
new.group=div.group[, c(1,13, 17:21) ]

library(rpart)
fit <-  rpart(SpeciesTotal~ cl_nest + cl_habitat + cl_food + cl_behav, method="anova", maxdepth=4, data=new.group)
printcp(fit)
par(xpd=NA,cex=0.8)
plot(fit)
text(fit)
